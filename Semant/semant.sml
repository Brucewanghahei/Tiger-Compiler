signature SEMANT =
sig
  type venv
  type tenv 
  type expty

  val transVar : venv * tenv * Absyn.var -> expty

  val transExp : venv * tenv * Absyn.exp -> expty

  val transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}

  val transTy : tenv * Absyn.ty -> Types.ty

  val transProg : Absyn.exp -> unit

end

structure Semant :> SEMANT =
struct

  (* alias *)
  structure Err = ErrorMsg
  structure A = Absyn
  structure Ty = Types
  structure E = Env
  structure S = Symbol
  val err = Err.error

  type venv = E.venv
  type tenv = E.tenv
  type expty = {exp: Translate.exp, ty: Types.ty}

  val loopLevel = ref 0
  val breakNum = ref 0

  (* use Ty.NIL as dummy return value *)
  fun actual_ty(ty) =
      case Ty.whatis ty of
          SOME t => t
        | NONE => (Err.impossible ("Type " ^ (Ty.name ty) ^ " is NONE"); Ty.NIL)

  fun lookActualType (tenv:tenv, symbol:S.symbol, pos) =
      case S.look(tenv, symbol) of
          SOME ty => actual_ty ty
        | NONE => (err pos ("Type " ^ S.name symbol ^ " not found"); Ty.NIL)

  fun compareAnyType(lhs: Ty.ty, rhs: Ty.ty) =
    case (lhs, rhs) of
         (Ty.NIL, Ty.NIL) => true
       | (Ty.INT, Ty.INT) => true
       | (Ty.STRING, Ty.STRING) => true
       | (Ty.UNIT, Ty.UNIT) => true
       | (Ty.NIL, Ty.RECORD(_, rhs_uni)) => true
       | (Ty.RECORD(_, lhs_uni), Ty.NIL) => true
       | (Ty.RECORD(_, lhs_uni), Ty.RECORD(_, rhs_uni)) => lhs_uni = rhs_uni
       | (Ty.ARRAY(_, lhs_uni), Ty.ARRAY(_, rhs_uni)) => lhs_uni = rhs_uni
       | (Ty.NAME a, Ty.NAME b) =>
           let
             val lhs_ty_opt = Ty.whatis(lhs)
             val rhs_ty_opt = Ty.whatis(rhs)
           in
             case (lhs_ty_opt, rhs_ty_opt) of
                  (SOME(lhs_ty), SOME(rhs_ty)) => compareAnyType(lhs_ty, rhs_ty)
                | (_, _) => false
           end
       | (_, _) => false 

  fun assertTypeEq (lhs: Ty.ty, rhs: Ty.ty, errCurry, msg) =
    if not (compareAnyType(actual_ty(lhs), actual_ty(rhs))) then
      errCurry msg
    else
      ()

  fun assertEq (lhs: 'a, rhs: 'a, eqFun, errCurry, msg) =
      if not (eqFun(lhs, rhs)) then
          errCurry msg
      else
          ()

  fun assertAssignable(venv, var: A.var, pos) = 
    case var of
      A.SimpleVar(symbol, _) => 
        (
        case S.look(venv, symbol) of 
          SOME(E.VarEntry {ty=_, assignable=assignable}) => 
            assertEq(assignable, true, op =, err pos, "variable not assignable: " ^ S.name symbol)
        | _ => ()
        )
    | _ => ()

  fun isValidRecord (Ty.RECORD(symTys, _)) =
      let 
          fun f (hd::tl) =
              if tl = nil then true
              else
                  let val hd2::tl2 = tl
                  in
                      hd <> hd2 andalso f tl
                  end
            | f nil = true
          val syms = ListMergeSort.sort op > (map (fn x => (S.getInt(#1 x))) symTys)
      in
          f syms
      end
    | isValidRecord _ = false

  (* look type until Ty.NAME *)
  fun lookType (tenv:tenv, symbol:S.symbol, pos) =
      case S.look(tenv, symbol) of
          NONE => (err pos ("Type " ^ S.name symbol ^ " not found"); Ty.NIL)
        | SOME ty => ty

  (* use Ty.INT as dummy return value *)
  fun lookupVariable(venv:venv, symbol:S.symbol, pos) =
    case S.look(venv, symbol) of
         SOME(E.VarEntry{ty=ty, assignable= _}) => {exp=(), ty=actual_ty(ty)}
       | _ => (err pos ("Undefined variable " ^ (S.name symbol)); {exp=(), ty=Ty.INT}) 

  (* use Ty.UNIT as dummy return value *)
  fun lookupRecordFieldType(sym_ty_list:((S.symbol * Ty.ty) list), id:S.symbol, pos) =
    case 
      (
      foldl
        (fn ((field, field_ty),(id, id_ty_opt)) =>
          (
          case id_ty_opt of
              NONE => if field = id then (id, SOME(field_ty))
                                                 else (id, NONE)  
            | SOME(id_ty) => (id, id_ty_opt) 
          )
        )
        (id, NONE) sym_ty_list
      ) of
      (id, SOME(field_ty)) => actual_ty(field_ty)
    | (id, NONE) => (
        err pos ("Field " ^ (S.name id) ^ " not found");
        Ty.UNIT
        )
      


  fun checkInt ({exp, ty}, pos, extra_info) = 
    assertEq (ty, Ty.INT, op =, err pos, "integer required: " ^ extra_info)
  
  fun checkNoValue ({exp, ty}, pos, extra_info) =
    assertEq (ty, Ty.UNIT, op =, err pos, "no-value required: " ^ extra_info)
    
  (* use E.FunEntry {...} as dummy return value *)
  (* If error, exit ? or return dummy entry ? *)
  fun lookupFunEntry (venv:venv, func:S.symbol, pos) = 
    case S.look(venv, func) of
         SOME fun_entry => fun_entry
       | NONE => (err pos ("Function " ^ (S.name func) ^ " not found"); E.FunEntry {formals = [], result = Ty.UNIT})

  fun checkFuncParams (func_name:string, formals: (S.symbol*Ty.ty) list, args: Ty.ty list, pos) = 
  let
    fun f ( (param_name, param_ty)::param_tail, input_ty::input_tail) =
        (
        assertTypeEq(param_ty, input_ty, err pos,
        "Parameter Mismatch:\n" ^ "function parameter: " ^ 
        (S.name param_name) ^ " : " ^(Ty.name param_ty)
        ^ "\ninput tpye: " ^ (Ty.name input_ty));
        f (param_tail, input_tail)
        )
      | f ([], []) = () 
      | f (_, _) = err pos ("Parameter size mismatch: function " ^ func_name) 
  in
    f(formals, args)
  end

  fun transVar(venv:venv, tenv:tenv, var:A.var) =
  let
    fun trvar(A.SimpleVar(sym, pos)) = lookupVariable(venv, sym, pos)
      | trvar(A.FieldVar(lvalue, sym, pos)) =
        let
          val {exp=_, ty=lvalue_ty} = transVar(venv, tenv, lvalue)
        in
          case lvalue_ty of
               Ty.RECORD (sym_ty_list, uni) =>
               {exp=(), ty=lookupRecordFieldType(sym_ty_list,
               sym, pos)}
             | _ => 
                (
                err pos "Record requried";
                {exp=(), ty=Ty.UNIT}
                )
        end
      | trvar(A.SubscriptVar(lvalue, exp, pos)) =
        let 
          val {exp=_, ty=lvalue_ty} = transVar(venv, tenv, lvalue)
        in
          case lvalue_ty of
            Ty.ARRAY (ty, uni) =>
              (
              checkInt(transExp(venv, tenv, exp), pos, "");
              {exp=(), ty=ty}          
              )
          | _ =>
              (
              err pos "Array required";
              {exp=(), ty=Ty.UNIT}
              )
        end
  in
    trvar var
  end

  and transExp(venv:venv, tenv:tenv, exp:A.exp) =
  let fun trexp (A.OpExp{left, oper, right, pos}) =
      let
        val {exp=_, ty=lty} = trexp left;
        val {exp=_, ty=rty} = trexp right;
      in
      (
        case oper of
          (A.EqOp | A.NeqOp | A.LtOp | A.LeOp | A.GtOp | A.GeOp) => ( 
            assertTypeEq(lty, rty, err pos, "left/right operand of OpExp type"^
            "mismatch\n" ^ "left operand: " ^ (Ty.name lty) ^"\nright operand: " ^
            (Ty.name rty));
            {exp=(), ty=Ty.INT}
          )
           | (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp) => (
            assertTypeEq(Ty.INT, lty, err pos, "left operand integer required");
            assertTypeEq(Ty.INT, rty, err pos, "right operand integer required");
            {exp=(), ty=Ty.INT}
          )
      )
      end
    | trexp (A.IntExp int) =
      {exp=(), ty=Ty.INT}
    | trexp (A.LetExp {decs, body, pos}) =
      let
        val {venv=venv', tenv=tenv'} =
        foldl (fn (a,b) => transDec(#venv b, #tenv b, a)) {venv=venv, tenv=tenv} decs
      in
        transExp(venv', tenv', body)
      end
    | trexp (A.SeqExp seq) =
      let
          fun f seq =
          case seq of
               [] => {exp=(), ty=Ty.UNIT}
             | [(exp, pos)] => trexp(exp)
             | (exp, pos)::tail =>
                 (
                 trexp(exp);
                 f(tail)
                 )
      in
        f seq
      end
    | trexp (A.ForExp {var=id, escape=escape, lo=lo, hi=hi, body=body, pos=pos}) =
      (
      checkInt(trexp lo, pos, S.name id);
      checkInt(trexp hi, pos, S.name id);
      let
        val _ = loopLevel := (!loopLevel) + 1
        val venv' = S.enter(venv, id, (E.VarEntry{ty = Ty.INT, assignable = false}))
        (*body check required*)
        val _ = loopLevel := (!loopLevel) - 1
        val _ = breakNum := 0
      in
        checkNoValue(transExp(venv', tenv, body), pos, " ForExp body should be UNIT") (* ensure id not re-assigned in the body scope *)
      end;
      {exp = (), ty=Ty.UNIT}
      )
    | trexp (A.VarExp var) =
      transVar(venv, tenv, var)
    | trexp (A.NilExp) =
      {exp=(), ty=Ty.NIL}
    | trexp (A.StringExp (str, pos)) =
      {exp=(), ty=Ty.STRING}        
    | trexp (A.CallExp {func, args, pos}) =
      (
      let
        val E.FunEntry{formals=formals, result=result} = lookupFunEntry(venv, func, pos)
        val () = checkFuncParams(S.name func, formals, map #ty (map trexp args), pos)
      in
        {exp=(), ty=result}
      end
      )
    | trexp (A.ArrayExp {typ, size, init, pos}) =
      let
        val {exp=_, ty=initTy} = trexp init
      in
        (
        checkInt(trexp size, pos, "Array size must be INT");
	    (case (lookActualType(tenv, typ, pos)) of
	      Ty.ARRAY(actTy, unique) =>
	      (assertTypeEq(actTy, initTy, err pos,
          "Type mismatch between initial type and array type.\n"
          ^ "Array type: " ^ (Ty.name (lookType(tenv, typ, pos)))
          ^ "\nInit type: " ^ (Ty.name initTy) );
		  {exp=(), ty=lookType(tenv, typ, pos)})
	    | _ => (err pos ("Invalid ARRAY type");
	        {exp=(), ty=Ty.UNIT})
        )
        )
      end
    | trexp (A.AssignExp {var, exp, pos}) =
      let
        val {exp=vExp, ty=varTy} = transVar (venv, tenv, var)
        val {exp=eExp, ty=expTy} = trexp exp
      in
        (
        assertAssignable(venv, var, pos); 
        assertTypeEq(varTy, expTy, err pos, "Assignment type mismatch");
        {exp=(), ty=Ty.UNIT}
        )
      end
    | trexp (A.WhileExp {test, body, pos}) =
      let
        val _ = loopLevel := !loopLevel + 1
        val _ = loopLevel := !loopLevel - 1
        val _ = breakNum := 0
      in
        (
        checkInt(trexp test, pos, "Invalid WHILE loop test type, INT expected");
        checkNoValue(trexp body, pos, "Invalid WHILE loop body type, UNIT expected");
        {exp=(), ty=Ty.UNIT}
        )
      end
    | trexp (A.BreakExp pos) =
      let
        val _ = breakNum := (!breakNum) + 1
      in
        (
        assertEq(!loopLevel, 0, op >, err pos, "Invalid BREAK");
        assertEq(!breakNum, 1, op =, err pos, "Can only BREAK once for a single loop");
        {exp=(), ty=Ty.UNIT}
        )
      end
    | trexp (A.IfExp {test=test, then'=then', else'=else_opt, pos=pos}) =
      (case else_opt of
           NONE =>
           (checkInt(trexp test, pos, "Invalid TEST expression type, INT expected");
            checkNoValue(trexp then', pos, "Invalid THEN expression type, UNIT expected");
            {exp=(), ty=Ty.UNIT})
      | SOME(else') =>
        let val thenTy = #ty (trexp then')
            val elseTy = #ty (trexp else')
        in
            checkInt(trexp test, pos, "Invalid TEST expression type, INT expected");
            assertTypeEq(thenTy, elseTy, err pos, "IfExp THEN-ELSE type mismatch");
            {exp=(), ty=thenTy}
        end
      )
    | trexp (A.RecordExp {fields = fields, typ = typ, pos = pos}) =
      let
          val fieldsTy = lookActualType(tenv, typ, pos)
          val fields1 = map (fn (symbol,exp,pos) => (symbol,
          (#ty (trexp(exp))))) fields 
          val names1 = map #1 fields1
          val types1 = map actual_ty (map #2 fields1)
      in
      (case fieldsTy of
          Types.RECORD(fields2, unique) =>
          let
              val names2 = map #1 fields2
              val types2 = map actual_ty (map #2 fields2)
          in
              if names1 = names2 then
                  if (ListPair.all
                      (fn (type1, type2) => compareAnyType (type1, type2))
                      (types1, types2))
                  then
                      {exp=(), ty=lookType(tenv, typ, pos)}
                  else
                      (err pos ("Inconsistent fields type: " ^ S.name typ);
                      {exp=(), ty=Ty.UNIT})
              else
                  (err pos ("Inconsistent fields type: " ^ S.name typ);
                  {exp=(), ty=Ty.UNIT})
          end
          | _ => (err pos ("Invalid RECORD type: " ^ S.name typ);
          {exp=(), ty=Ty.UNIT}))
      end
    in
      trexp exp
    end

    and transDec (venv:venv, tenv:tenv, dec:A.dec) =
      let fun trdec (A.VarDec{name, typ = NONE, init, pos, ...}) =
              let val {exp, ty} = transExp(venv, tenv, init)
                  val msgTmpl = "VarDec: "
              in
                  assertEq(ty, Ty.NIL, op <>, err pos, msgTmpl ^ S.name name ^ " cannot be assigned to nil implicitly");
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = ty, assignable =
                    true}),
                    tenv = tenv
                  }
              end
            | trdec (A.VarDec{name, typ = SOME(s, _), init, pos, ...}) = 
              let val {exp, ty} = transExp(venv, tenv, init)
                  val msgTmpl = "VarDec: "
                  val decTy = lookType(tenv, s, pos)
              in
                  assertTypeEq(ty, decTy, err pos, msgTmpl ^ S.name name
                  ^ " - type mismatch\nVariable type: " ^ (Ty.name decTy) 
                  ^ "\nInit type: " ^ (Ty.name ty));
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = decTy, assignable
                    = true}),
                    tenv = tenv
                  }
              end
            | trdec (A.TypeDec(tydecs)) =
              let
                  val msgTmpl = "TypeDec: "
                                    
                  (* first pass to scan headers*)
                  fun trTyDecHeaders (tenv:tenv, tydecs) =
                      foldl (fn ({name, ty, pos}, acc) => S.enter(acc, name, Types.NAME(name, ref NONE))) tenv tydecs
                  (* second pass to fill ref *)

                  fun trTyDecBody (tenv, {name, ty, pos}) =
                      let val tyHeader = transTy(tenv, ty)
                      in
                        case lookType(tenv, name, pos) of
                            (Ty.NAME (_, tyref)) => tyref := SOME tyHeader
                            | _ => Err.impossible (msgTmpl ^ S.name name ^ " not found in header")
                      end

                  (* check alias cycle: ignore array/record *)
                  fun checkTyDecCycle (tenv, tydecs) =
                      let
                          val nodes = map (fn {name, ty, pos} => name) tydecs
                          fun dfs (node, parent, start) =
                              if node = parent orelse node = start then
                                  true
                              else
                                  dfsHelper(node, start)
                          and dfsHelper (node, start) =
                              case lookType(tenv, node, 0) of
                                  Ty.NAME(snode, tyref) =>
                                  (
                                    case !tyref of
                                        SOME ty =>
                                        (
                                          case ty of Ty.NAME(snext, _) =>
                                                     dfs(snext, snode, start)
                                                  | _ => false
                                        )
                                      | _ => false
                                  )
                                | _ => false
                      in
                          List.exists (fn node => dfsHelper(node, node)) nodes
                      end

                  val tenv' = trTyDecHeaders(tenv, tydecs)
              in
                  map (fn dec => trTyDecBody(tenv', dec)) tydecs;
                  assertEq(checkTyDecCycle(tenv', tydecs), false, op =, err 0, msgTmpl ^ " cycle(no record/array) detected in mutual recursion");
                  {
                    venv = venv,
                    tenv = tenv'
                  }
              end
            | trdec (A.FunctionDec fundecs) =
              let
                  val msgTmpl = "FunDec: "
                  fun trParams params = map (fn {name, typ, pos, escape} =>
                                        (name, lookType(tenv,typ, pos)))
                                        params
                  (* first pass to scan headers*)

                  fun trResult NONE = Ty.UNIT
                    | trResult (SOME(s, pos)) = 
                      case lookActualType(tenv, s, pos) of
                          Ty.NIL => (err pos(msgTmpl ^ "return value cannot be given as nil"); Ty.NIL)
                        | t => t

                  fun trFunDecHeaders (venv:venv, decs) =
                      foldl (fn ({name, params, result, body, pos}, acc) =>
                                let
                                    val resultTy = trResult result
                                    val paramNameTys = trParams params
                                in
                                    S.enter(acc, name, E.FunEntry{formals = paramNameTys, result = resultTy})
                                end)
                            venv
                            decs

                  val venv' = trFunDecHeaders(venv, fundecs)

                  (* second pass to translate body*)
                  fun trFunDecBody (venv:venv, {name, params, result, body, pos}: A.fundec) =
                      let
                          val resultTy = trResult result
                          val paramNameTys = trParams params
                          fun enterParam ((name, ty), venv:venv) =
                              S.enter(venv, name, E.VarEntry{ty = ty, assignable = false})
                          val venv' = foldl enterParam venv paramNameTys
                          val bodyTy = #ty (transExp(venv', tenv, body))
                      in
                          assertTypeEq(bodyTy, resultTy, err pos, msgTmpl ^
                          (S.name name) ^ " type mismatch.\nReturn type: " ^
                          (Ty.name resultTy) ^ "\nBody type: " ^ (Ty.name
                          bodyTy))
                      end
              in
                  map (fn dec => trFunDecBody(venv', dec)) fundecs;
                  {
                    venv = venv',
                    tenv = tenv
                  }
              end
      in
          trdec dec
      end
      
    and transTy (tenv:tenv, ty:A.ty) =
    let
      fun trty (A.NameTy (sym, pos)) = lookType(tenv, sym, pos)
        | trty (A.RecordTy (field_list)) = Ty.RECORD ((map (fn {name, escape, typ, pos} =>
            (name, lookType(tenv, typ, pos))) field_list), ref ()) 
        | trty (A.ArrayTy (sym, pos)) = Ty.ARRAY (lookType(tenv, sym,
        pos), ref ())
    in
      trty ty
    end
    
    and transProg (exp) =
    (
    transExp(E.base_venv, E.base_tenv, exp);
    ()
    )
end
