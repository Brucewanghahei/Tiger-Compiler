signature SEMANT =
sig
  type venv
  type tenv 
  type expty

  val transVar : venv * tenv * Absyn.var * Translate.level -> expty

  val transExp : venv * tenv * Absyn.exp * Translate.level * Temp.label -> expty

  val transDec : venv * tenv * Absyn.dec * Translate.level * Temp.label -> {venv: venv, tenv: tenv, exps: Translate.exp list}

  val transTy : tenv * Absyn.ty -> Types.ty

  val transProg : Absyn.exp -> Translate.frag list

end

structure Semant :> SEMANT =
struct

  (* alias *)
  structure Err = ErrorMsg
  structure A = Absyn
  structure Ty = Types
  structure Tp = Temp
  structure E = Env
  structure S = Symbol
  structure R = Translate
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
          SOME(E.VarEntry {access, ty, assignable}) => 
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

  fun lookupVariable(venv:venv, symbol:S.symbol, pos) =
    case S.look(venv, symbol) of
         SOME v => SOME v
       | NONE => (err pos ("Undefined variable " ^ (S.name symbol)); NONE)

  (* use Ty.UNIT as dummy return value *)
  fun lookupRecordField(sym_ty_list:((S.symbol * Ty.ty) list), id:S.symbol, pos)
    : (Ty.ty * int) =
    case 
      (
      foldl
        (fn ((field, field_ty), (id, id_ty_opt, k)) =>
          (
          case id_ty_opt of
              NONE => if field = id then (id, SOME(field_ty), k)
                                                 else (id, NONE, k+1)  
            | SOME(id_ty) => (id, id_ty_opt, k) 
          )
        )
        (id, NONE, 0) sym_ty_list
      ) of
      (id, SOME(field_ty), k) => (field_ty, k)
    | (id, NONE, _) => (
        err pos ("Field " ^ (S.name id) ^ " not found");
        (Ty.UNIT, 0)
        )
      


  fun checkInt ({exp, ty}, pos, extra_info) = 
    assertTypeEq (ty, Ty.INT, err pos, "integer required: " ^ extra_info)
  
  fun checkNoValue ({exp, ty}, pos, extra_info) =
    assertTypeEq (ty, Ty.UNIT, err pos, "no-value(UNIT) required: " ^ extra_info)
    
  fun lookupFunEntry (venv:venv, func:S.symbol, pos) = 
    case S.look(venv, func) of
         SOME fun_entry => SOME fun_entry
       | NONE => (err pos ("Function " ^ (S.name func) ^ " not found"); NONE)

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

  fun transVar(venv:venv, tenv:tenv, var:A.var, level) =
  let
      fun trvar(A.SimpleVar(sym, pos)) = ( 
          case lookupVariable(venv, sym, pos) of
              SOME {access, ty, assignable} => {exp=R.simpleVar(access, level), ty=ty}
            | NONE => {exp=R.dummy_exp,
            ty=Ty.INT} ) (* dummy pattern match*)
      | trvar(A.FieldVar(lvalue, sym, pos)) =
        let
          val {exp=base_pointer, ty=lvalue_ty} = transVar(venv, tenv, lvalue)
        in
          case actual_ty lvalue_ty of
            Ty.RECORD (sym_ty_list, uni) =>
            let
              val (ty, k) = lookupRecordFieldType(sym_ty_list, sym, pos)
            in
              {exp=R.subVar(base_pointer, R.intlit(k)), ty=ty}
            end
          | _ => 
            (
            err pos "Record requried";
            {exp=R.dummy_exp, ty=Ty.UNIT}
            )
        end
      | trvar(A.SubscriptVar(lvalue, exp, pos)) =
        let 
          val {exp=base_pointer, ty=lvalue_ty} = transVar(venv, tenv, lvalue)
        in
          case actual_ty lvalue_ty of
            Ty.ARRAY (ty, uni) =>
            let
              val {exp=k_exp, ty=ty} = transExp(venv, tenv, exp)
              val _ = assertTypeEq(ty, Ty.INT, err pos, "Array index type should integer");
            in
              {exp=R.subVar(base_pointer, k_exp), ty=ty}          
            end
          | _ =>
              (
              err pos "Array required";
              {exp=R.dummy_exp, ty=Ty.UNIT}
              )
        end
  in
    trvar var
  end

  and transExp(venv:venv, tenv:tenv, exp:A.exp, level, breakLabel) =
  let fun trexp (A.OpExp{left, oper, right, pos}) =
      let
        val {exp=lexp, ty=lty} = trexp left;
        val {exp=rexp, ty=rty} = trexp right;
      in
      (
        case oper of
          (A.EqOp | A.NeqOp | A.LtOp | A.LeOp | A.GtOp | A.GeOp) => ( 
            assertTypeEq(lty, rty, err pos, "left/right operand of OpExp type" ^
            " mismatch\n" ^ "left operand: " ^ (Ty.name lty) ^"\nright operand: " ^
            (Ty.name rty));
            {exp=R.compOp(oper, lexp, rexp), ty=Ty.INT}
          )
           | (A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp) => (
            assertTypeEq(Ty.INT, lty, err pos, "left operand integer required");
            assertTypeEq(Ty.INT, rty, err pos, "right operand integer required");
            {exp=R.arithOp(oper, lexp, rexp), ty=Ty.INT}
          )
      )
      end
    | trexp (A.IntExp x) =
      {exp=R.intlit x, ty=Ty.INT}
    | trexp (A.LetExp {decs, body, pos}) =
      let
        val {venv=venv', tenv=tenv', exps=dec_exps} =
          foldl (fn (a,b) => 
          let
            val {venv=venv, tenv=tenv, exps=exps} = 
            transDec(#venv b, #tenv b, a, level, breakLabel)
          in
            {venv=venv, tenv=tenv, exps=(#exps b)@exps}
          end)
          {venv=venv, tenv=tenv, exps=[]} decs
        {exp=body_exp, ty=body_ty} = transExp(venv', tenv', body)
      in
        {exp=R.letexp(dec_exps, body_exp), ty=body_ty}
      end
    | trexp (A.SeqExp seq) =
      let
          fun f seq =
          case seq of
               [] => {exp=R.dummy_exp, ty=Ty.UNIT}
             | [(exp, pos)] => trexp(exp)
             | (exp, pos)::tail =>
                 (
                 trexp(exp);
                 f(tail)
                 )
      in
        f seq
      end
    | trexp (A.ForExp {var=for_sym, escape=escape, lo=lo, hi=hi, body=body, pos=pos}) =
      let 
        val lo_exp = trexp lo
        val hi_exp = trexp hi

        val for_access = R.allocLocal level (! escape)
        val venv' = S.enter(venv, for_sym, (E.VarEntry{access=for_access, ty = Ty.INT, assignable = false}))

        val _ = loopLevel := (!loopLevel) + 1
        val _ = breakNum := 0
        val body_exp = transExp(venv', tenv, body, level, breakLabel)
        val _ = loopLevel := (!loopLevel) - 1
      in
        checkInt(lo_exp, pos, "Low side of the ForExp with variable: " ^ S.name id );
        checkInt(hi_exp, pos, "High side of the ForExp with variable: " ^ S.name id);
        checkNoValue(body_exp, pos, " ForExp body should be UNIT"); (* ensure id not re-assigned in the body scope *)
        {exp=R.forExp(for_access, #exp lo_exp, #exp hi_exp, #exp body_exp), ty=Ty.UNIT}
      end
    | trexp (A.VarExp var) =
      transVar(venv, tenv, var, level)
    | trexp (A.NilExp) =
      {exp=R.nilkw, ty=Ty.NIL}
    | trexp (A.StringExp (str, pos)) =
      {exp=R.strlit str, ty=Ty.STRING}        
    | trexp (A.CallExp {func, args, pos}) =
      (
      let
        val entry = lookupFunEntry(venv, func, pos)
        val result = case entry of SOME {result, ...} => result
                                 | NONE => Ty.NIL
      in
          case entry of
               SOME {funLevel, label, formals, result} =>
               (checkFuncParams(S.name func, formals, map #ty (map trexp args), pos);
                {
                  exp = R.call(level, funLevel, label, map (fn arg => #exp (trexp arg)) args),
                  ty = result
               })
             | NONE => {
                 exp = R.dummy_exp,
                 ty = result
             }
      end
      )
    | trexp (A.ArrayExp {typ, size, init, pos}) =
      let
        val {exp=init_exp, ty=initTy} = trexp init
        val {exp=size_exp, ty=sizeTy} = trexp size
      in
        (
        assertTypeEq(sizeTym, Ty.INT, err pos, "Array size must be INT");
	    (case (lookActualType(tenv, typ, pos)) of
	      Ty.ARRAY(actTy, unique) =>
	      (assertTypeEq(actTy, initTy, err pos,
          "Type mismatch between initial type and array type.\n"
          ^ "Array type: " ^ (Ty.name (lookType(tenv, typ, pos)))
          ^ "\nInit type: " ^ (Ty.name initTy) );
		  {exp=R.createArray(init_exp, size_exp), ty=lookType(tenv, typ, pos)})
	    | _ => (err pos ("Invalid ARRAY type");
	        {exp=R.dummy_exp, ty=Ty.UNIT})
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
        {exp=R.assign(vExp, eExp), ty=Ty.UNIT}
        )
      end
    | trexp (A.WhileExp {test, body, pos}) =
      let
        val _ = loopLevel := !loopLevel + 1
	val {exp=testExp, ty=_} = transExp (venv, tenv, test, level, breakLabel)
	val {exp=bodyExp, ty=_} = transExp (venv, tenv, body, level, breakLable)
        val _ = loopLevel := !loopLevel - 1
        val _ = breakNum := 0
      in
        (
        checkInt(trexp test, pos, "Invalid WHILE loop test type, INT expected");
        checkNoValue(trexp body, pos, "Invalid WHILE loop body type, UNIT expected");
        {exp=(R.whileExp(testExp, bodyExp)), ty=Ty.UNIT}
        )
      end
    | trexp (A.BreakExp pos) =
      let
        val _ = breakNum := (!breakNum) + 1
      in
        (
        assertEq(!loopLevel, 0, op >, err pos, "Invalid BREAK");
        assertEq(!breakNum, 1, op =, err pos, "Can only BREAK once for a single loop");
        {exp=(R.breakExp(breakLabel)), ty=Ty.UNIT}
        )
      end
    | trexp (A.IfExp {test=test, then'=then', else'=else_opt, pos=pos}) =
      (case else_opt of
          NONE =>
              (checkInt(trexp test, pos, "Invalid TEST expression type, INT expected");
              checkNoValue(trexp then', pos, "Invalid THEN expression type, UNIT expected");
              {exp=(#exp (trexp test), #exp (trexp then')), ty=Ty.UNIT})
          | SOME(else') =>
              (checkInt(trexp test, pos, "Invalid TEST expression type, INT expected");
              assertTypeEq(#ty (trexp then'), #ty (trexp else'), err pos, "IfExp THEN-ELSE type mismatch");
              {exp=(#exp (trexp test), #exp (trexp then'), #exp (trexp else')), ty=thenTy}))
    | trexp (A.RecordExp {fields = fields, typ = typ, pos = pos}) =
      let
        val recordTy = lookActualType(tenv, typ, pos)
        val ini_fields = map (fn (symbol,exp,pos) => (symbol, trexp exp, pos)) fields 
        val ini_exps = map #exp (#2 ini_fields) 
      in
      (case recordTy of
          Types.RECORD(def_fields, unique) =>
          let
            fun check (ini_field::ini_tail, def_field::def_tail) =
            let
              val ini_id = #1 ini_field and def_id = #1 def_field
              val ini_ty = #ty (#2 ini_field) and def_ty = #2 def_field
              val pos = #3 ini_fields
            in
              if compareAnyType(ini_ty, ini_id) andalso ini_id = def_id then
                check (ini_tail, def_tail)
              else
                (err pos ("Inconsistent fields type: " ^ S.name typ);
                false)
            end
              | check ([], [_]) = (err pos ("Need more fileds for the record: "
              ^ S.name typ); false)
              | check ([_], []) = (err pos ("Too many fileds for the record: "
              ^ S.name typ); false)
              | check ([], []) = true
          in
            if check(ini_fields, def_fields) then
              {exp=R.createRecord(ini_exps), ty=lookType(typ)}
            else
              {exp=R.dummy_exp, ty=Ty.UNIT}
          end
          | _ => (err pos ("Invalid RECORD type: " ^ S.name typ);
          {exp=R.dummy_exp, ty=Ty.UNIT}))
      end
    in
      trexp exp
    end

    and transDec (venv, tenv, dec, level, breakLabel) =
      let fun trdec (A.VarDec{name, escape, typ, init, pos, ...}) =
              let val {exp, ty} = transExp(venv, tenv, init)
                  val msgTmpl = "VarDec: "
                  (* translate *)
                  val access = R.allocLocal level !escape
                  val varexp = R.simpleVar(access, level)
              in
                  case typ of
                      SOME(s, _) =>
                      let 
                          val decTy = lookType(tenv, s, pos)
                      in
                          assertTypeEq(ty, decTy, err pos, msgTmpl ^ S.name name
                                                           ^ " - type mismatch\nVariable type: " ^ (Ty.name decTy) 
                                                           ^ "\nInit type: " ^ (Ty.name ty))
                      end
                    | NONE => assertEq(actual_ty ty, Ty.NIL, op <>, err pos, msgTmpl ^ S.name name ^ " cannot be assigned to nil implicitly")
                ;
                        {
                          venv = S.enter(venv, name, E.VarEntry{ty = ty, assignable = true}),
                          tenv = tenv,
                          exps = [R.assign(varexp, exp)]
                        }
              end
            | trdec (A.TypeDec(tydecs)) =
              let
                  val msgTmpl = "TypeDec: "
                  (* first pass to scan headers*)
                  fun trTyDecHeaders (tenv:tenv, tydecs) =
                  let
                    val temp_tenv = S.empty;
                    val (new_tenv, tmp_tenv) =
                      foldl (fn ({name, ty, pos}, (acc, acc_temp)) => (
                      S.enter(acc, name,
                      Types.NAME(name, ref NONE)), 
                      (case S.look(acc_temp, name) of
                           SOME ty =>  (err pos (msgTmpl ^ "same mutually recursive type name"); acc_temp)
                         | NONE => S.enter(acc_temp, name, Ty.UNIT))
                      )
                      ) (tenv, temp_tenv) tydecs
                  in
                    new_tenv
                  end
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
                    tenv = tenv',
                    exps = []
                  }
              end
            | trdec (A.FunctionDec fundecs) =
              let
                  val msgTmpl = "FunDec: "
                  fun trParams (params, level) =
                      {
                        nameTys = (map #name params,
                                   map (fn {typ, ...} => lookType(tenv, typ, pos)) params),
                        escapes = map (fn {escape, ...} => !escape)  params,
                        accesses = R.formals level
                      }
                  (* first pass to scan headers*)

                  fun trResult NONE = Ty.UNIT
                    | trResult (SOME(s, pos)) = 
                      case lookActualType(tenv, s, pos) of
                          Ty.NIL => (err pos(msgTmpl ^ "return value cannot be given as nil"); Ty.NIL)
                        | t => t

                  fun trFunDecHeaders (venv:venv, decs) =
                      let
                          val (venv', _) =
                              foldl
                                  (fn ({name, params, result, body, pos}, (acc, accTemp)) =>
                                      let
                                          val resultTy = trResult result
                                          val {nameTys, escapes, ...} = trParams params
                                          val newLabel = Tp.newLable
                                          val entry = E.FunEntry{
                                                  level = R.newLevel{parent = level, name = newLable, escapes = escapes},
                                                  label = newLabel,
                                                  formals = nameTys,
                                                  result = resultTy
                                              }
                                      in
                                          (
                                            S.enter(acc, name, entry),
                                            case S.look(accTemp, name) of
                                                SOME _ => (err ~1
                                                               (msgTmpl ^ "same mutually recursive function declaration");
                                                           accTemp)
                                              | NONE => S.enter(accTemp, name, entry)
                                          )
                                      end)
                                  (venv, S.empty)
                                  decs
                      in
                          venv'
                      end

                  val venv' = trFunDecHeaders(venv, fundecs)

                  (* second pass to translate body*)
                  fun trFunDecBody (venv:venv, {name, params, result, body, pos}: A.fundec) =
                      let
                          val resultTy = trResult result
                          val {nameTys, accesses, ...} = trParams params
                          fun enterParam (((name, ty), access), venv:venv) =
                              S.enter(venv, name, E.VarEntry{access = access, ty = ty, assignable = true})
                          val venv' = foldl enterParam venv ListPair.zip(nameTys, accesses)
                          val {exp = bodyExp, ty = bodyTy} = transExp(venv', tenv, body)
                      in
                          assertTypeEq(bodyTy, resultTy, err pos, msgTmpl ^
                          (S.name name) ^ " type mismatch.\nReturn type: " ^
                          (Ty.name resultTy) ^ "\nBody type: " ^ (Ty.name
                          bodyTy))
                        ;
                          (case lookupFunEntry(venv, name, pos) of
                                SOME{level, ...} => R.procEntryExit1(bodyExp, level))
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
        let val mainlevel = R.newLevel {parent = R.outermost, name = Temp.namedlabel "main", escapes = []}
        in
            transExp(E.base_venv, E.base_tenv, exp, mainlevel, Temp.newLabel());
            ()
        end
end
