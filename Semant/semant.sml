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
  
  fun compareAnyType(lhs: Ty.ty, rhs: Ty.ty) =
    case (lhs, rhs) of
         (Ty.NIL, Ty.NIL) => true
       | (Ty.INT, Ty.INT) => true
       | (Ty.STRING, Ty.STRING) => true
       | (Ty.UNIT, Ty.UNIT) => true
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
    if not (compareAnyType(lhs, rhs)) then
      errCurry msg
    else
      ()

  fun assertTypeMatch (ty, decTy, errCurry, msg) =
      case (ty, decTy) of
          (Ty.NIL, Ty.RECORD(_, _)) => ()
        | _ => assertTypeEq(ty, decTy, errCurry, msg)

  fun assertEq (lhs: 'a, rhs: 'a, eqFun, errCurry, msg) =
      if not (eqFun(lhs, rhs)) then
          errCurry msg
      else
          ()

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

  (* use Ty.NIL as dummy return value *)
  fun actual_ty(ty) =
      case Ty.whatis ty of
          SOME t => t
        | NONE => (Err.impossible ("Type " ^ (Ty.name ty) ^ " is NONE"); Ty.NIL)

  fun lookActualType (tenv:tenv, symbol:S.symbol, pos) =
      case S.look(tenv, symbol) of
          SOME ty => actual_ty ty
        | NONE => (err pos ("Type " ^ S.name symbol ^ " not found"); Ty.NIL)

  fun lookHeaderType (tenv:tenv, symbol:S.symbol) =
      case S.look(tenv, symbol) of
          SOME ty => ty
        | NONE => (Err.impossible (" Header type " ^ S.name symbol ^ " not found"); Ty.NIL)

  (* use Ty.INT as dummy return value *)
  fun lookupVariable(venv:venv, symbol:S.symbol, pos) =
    case S.look(venv, symbol) of
         SOME(E.VarEntry{ty}) => {exp=(), ty=actual_ty(ty)}
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
        err pos ("Field" ^ (S.name id) ^ " not found");
        Ty.UNIT
        )
      


  fun checkInt ({exp, ty}, pos, extra_info) = 
    assertEq (ty, Ty.INT, op =, err pos, "integer required" ^ extra_info)
  
  fun checkNoValue ({exp, ty}, pos, extra_info) =
    assertEq (ty, Ty.UNIT, op =, err pos, "no-value required" ^ extra_info)
    
  (* use E.FunEntry {...} as dummy return value *)
  (* If error, exit ? or return dummy entry ? *)
  fun lookupFunEntry (venv:venv, func:S.symbol, pos) = 
    case S.look(venv, func) of
         SOME fun_entry => fun_entry
       | NONE => (err pos ("Function " ^ (S.name func) ^ " not found"); E.FunEntry {formals = [], result = Ty.UNIT})

  fun checkFuncParams (formals: Ty.ty list, args: Ty.ty list, pos) = 
  let
    fun f (lhs_head::lhs_tail, rhs_head::rhs_tail) =
        (
        assertTypeEq(lhs_head, rhs_head, err pos,
        "Parameter Mismatch:\n" ^ "function parameter: " ^ (Ty.name lhs_head) ^
        "input parameter: " ^ (Ty.name rhs_head));
        f (lhs_tail, rhs_tail)
        )
      | f ([], []) = () 
      | f (_, _) = err pos "Parameter size mismatch"
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
      (
      checkInt(trexp left, pos, "");
      checkInt(trexp right, pos, "");
      {exp=(), ty=Ty.INT}
      )
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
               [] => (
                 err ~1 "two or more expression in seq requried";
                 {exp=(), ty=Ty.UNIT}
                 )
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
      checkInt(trexp lo, pos, "");
      checkInt(trexp hi, pos, "");
      let
        val _ = loopLevel := (!loopLevel) + 1
        val venv' = S.enter(venv, id, (E.VarEntry{ty = Ty.UNIT}))
        (*body check required*)
        val _ = loopLevel := (!loopLevel) - 1
        val _ = breakNum := 0
      in
        checkNoValue(transExp(venv', tenv, body), pos, "") (* ensure id not re-assigned in the body scope *)
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
        val () = checkFuncParams(formals, map #ty (map trexp args), pos)
      in
        {exp=(), ty=result}
      end
      )
    | trexp (A.ArrayExp {typ, size, init, pos}) =
      let
        val {exp=_, ty=initTy} = trexp init
        val array_ele_ty = lookActualType(tenv, typ, pos)
      in
        (
        checkInt(trexp size, pos, "Array size must be INT");
        assertTypeEq(array_ele_ty, initTy, err pos, "Array initial type does not match base type");
        {exp=(), ty=Ty.ARRAY(array_ele_ty, ref ())}
        )
      end
    | trexp (A.AssignExp {var, exp, pos}) =
      let
        val {exp=vExp, ty=varTy} = transVar (venv, tenv, var)
        val {exp=eExp, ty=expTy} = trexp exp
      in
        (
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
              (checkInt(trexp test, pos, "Invalid TEST expression type, INT expected");
          checkNoValue(trexp then', pos, "Invalid THEN expression type, UNIT expected");
          checkNoValue(trexp else', pos, "Invalid ELSE expression type, UNIT expected");
          {exp=(), ty=Ty.UNIT})
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
                      {exp=(), ty=fieldsTy}
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
                    venv = S.enter(venv, name, E.VarEntry{ty = ty}),
                    tenv = tenv
                  }
              end
            | trdec (A.VarDec{name, typ = SOME(s, _), init, pos, ...}) = 
              let val {exp, ty} = transExp(venv, tenv, init)
                  val msgTmpl = "VarDec: "
                  val decTy = lookActualType(tenv, s, pos)
              in
                  assertTypeEq(ty, decTy, err pos, msgTmpl ^ S.name name ^ " - type mismatch");
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = decTy}),
                    tenv = tenv
                  }
              end
            | trdec (A.TypeDec(tydecs)) =
              let
                  val msgTmpl = "TypeDec: "
                  (* first pass to scan headers*)
                  fun trTyDecHeader (tenv:tenv, tydecs) =
                      foldl (fn ({name, ty, pos}, acc) => S.enter(acc, name, Types.NAME(name, ref NONE))) tenv tydecs
                  (* second pass to fill ref *)

                  fun trTyDecBody (tenv, {name, ty, pos}::tl) =
                      let val actualTy = transTy(tenv, ty)
                      in
                        case lookHeaderType(tenv, name) of
                            (Ty.NAME (_, tyref)) => tyref := SOME actualTy
                            | _ => Err.impossible (msgTmpl ^ S.name name ^ " not found in header")
                          ;
                            {
                              venv = venv,
                              tenv = tenv
                            }
                      end
                    | trTyDecBody (tenv:tenv, nil) =
                      {
                        venv = venv,
                        tenv = tenv
                      }
                  val tenv' = trTyDecHeader(tenv, tydecs)
              in
                  trTyDecBody(tenv', tydecs)
              end
            | trdec (A.FunctionDec fundecs) =
              let
                  val msgTmpl = "FunDec: "
                  fun trParams params = map (fn {name, typ, pos, escape} =>
                                                               {name = name, ty = lookActualType(tenv, typ, pos)})
                                                           params
                  (* first pass to scan headers*)

                  fun trResult NONE = Ty.NIL
                    | trResult (SOME(s, pos)) = 
                      case lookActualType(tenv, s, pos) of
                          Ty.NIL => (err pos(msgTmpl ^ "return value cannot be given as nil"); Ty.NIL)
                        | t => t

                  fun trFunDecHeader (venv:venv, {name, params, result, body, pos}::tl) =
                      let
                          val resultTy = trResult result
                          val paramNameTys = trParams params
                          val venv' = S.enter(venv, name,
                                              E.FunEntry{formals = map #ty paramNameTys, result = resultTy})
                      in
                          trFunDecHeader(venv', tl)
                      end
                    | trFunDecHeader (venv, nil) = venv

                  val venv' = trFunDecHeader(venv, fundecs)

                  (* second pass to translate body*)
                  fun trFunDec (venv:venv, {name, params, result, body, pos}::tl: A.fundec list) =
                      let
                          val resultTy = trResult result
                          val paramNameTys = trParams params
                          fun enterParam ({name, ty}, venv:venv) =
                              S.enter(venv, name, E.VarEntry{ty = ty})
                          val venv' = foldl enterParam venv paramNameTys
                          val bodyTy = #ty (transExp(venv', tenv, body))
                      in
                          if resultTy <> Ty.NIL then
                              assertTypeMatch(bodyTy, resultTy, err pos, msgTmpl ^ S.name name ^ "type mismatch")
                          else
                              ()
                        ;
                          venv
                      end
                    | trFunDec (venv, nil) = venv
              in
                  {
                    venv = trFunDec(venv', fundecs),
                    tenv = tenv
                  }
              end
      in
          trdec dec
      end
      
    and transTy (tenv:tenv, ty:A.ty) =
    let
      fun trty (A.NameTy (sym, pos)) = lookActualType(tenv, sym, pos)
        | trty (A.RecordTy (field_list)) = Ty.RECORD ((map (fn {name, escape, typ, pos} =>
            (name, lookActualType(tenv, typ, pos))) field_list), ref ()) 
        | trty (A.ArrayTy (sym, pos)) = Ty.ARRAY (lookActualType(tenv, sym,
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
