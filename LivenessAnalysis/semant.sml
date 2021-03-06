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
       | (Ty.NAME a, r) => compareAnyType(actual_ty(lhs), r)
       | (l, Ty.NAME b) => compareAnyType(l, actual_ty(rhs))
       | (_, _) => false 

  fun assertTypeEq (lhs: Ty.ty, rhs: Ty.ty, errCurry, msg) =
    if not (compareAnyType(actual_ty(lhs), actual_ty(rhs))) then
      errCurry (msg^"\nLtype="^(Ty.name lhs)^" Rtype="^(Ty.name rhs))
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
      | f ([_], []) = err pos ("Function " ^ func_name ^ ": Need more input parameters")
      | f ([], [_]) = err pos ("Function " ^ func_name ^ ": Too many input parameters")
      | f ([], []) = () 
  in
    f(formals, args)
  end

  fun transVar(venv:venv, tenv:tenv, var:A.var, level) =
  let
      fun trvar(A.SimpleVar(sym, pos)) = ( 
          case lookupVariable(venv, sym, pos) of
              SOME (E.VarEntry{access=access, ty=ty, ...}) => {exp=R.simpleVar(access, level), ty=ty}
            | SOME _ => ((err pos "exptected variable, found function"); {exp=R.dummy_exp, ty=Ty.NIL})
            | NONE => (err pos ("Variable not found: " ^ (A.VarName (A.SimpleVar(sym, pos))));
                  {exp=R.dummy_exp, ty=Ty.NIL} )) (* dummy pattern match*)
      | trvar(A.FieldVar(lvalue, sym, pos)) =
        let
          val {exp=base_pointer, ty=lvalue_ty} = transVar(venv, tenv, lvalue, level)
        in
          case actual_ty lvalue_ty of
            Ty.RECORD (sym_ty_list, uni) =>
            let
              val (ty, k) = lookupRecordField(sym_ty_list, sym, pos)
            in
              {exp=R.subVar(base_pointer, R.intlit(k)), ty=ty}
            end
          | _ => 
            (
            err pos ("Variable " ^ (A.VarName (A.FieldVar(lvalue, sym, pos))) ^
            " : Record type requried.\nGiven type: " ^ (Ty.name lvalue_ty));
            {exp=R.dummy_exp, ty=Ty.UNIT}
            )
        end
      | trvar(A.SubscriptVar(lvalue, exp, pos)) =
        let 
          val {exp=base_pointer, ty=lvalue_ty} = transVar(venv, tenv, lvalue, level)
        in
          case actual_ty lvalue_ty of
            Ty.ARRAY (ty, uni) =>
            let
              val {exp=k_exp, ty=k_ty} = transExp(venv, tenv, exp, level, Temp.newlabel())
              val _ = assertTypeEq(k_ty, Ty.INT, err pos, "Array index type should integer");
            in
              {exp=R.subVar(base_pointer, k_exp), ty=ty}          
            end
          | _ =>
              (
              err pos ("Variable " ^ (A.VarName (A.SubscriptVar(lvalue, exp, pos)))
              ^ " : Array type required.\nGiven type: "^(Ty.name lvalue_ty));
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
        val {exp=body_exp, ty=body_ty} = transExp(venv', tenv', body, level, breakLabel)
      in
        {exp=R.letexp(dec_exps, body_exp), ty=body_ty}
      end
    | trexp (A.SeqExp seq) =
      let
          fun f seq_list =
            foldl (fn ((abs_seq, pos_seq), (ir_exp_list, tail_ty)) => 
            let
              val {exp=ir_exp, ty=seq_ty} = trexp abs_seq
            in
              (ir_exp :: ir_exp_list, seq_ty)
            end) ([], Ty.UNIT) seq_list
          val (ir_exp_list, tail_ty) = f seq
      in
        {exp=R.seqexp(ir_exp_list), ty=tail_ty}
      end
    | trexp (A.ForExp {var=for_sym, escape=escape, lo=lo, hi=hi, body=body, pos=pos}) =
      let 
        val lo_exp = trexp lo
        val hi_exp = trexp hi

        val for_access = R.allocLocal level (! escape)
        val venv' = S.enter(venv, for_sym, (E.VarEntry{access=for_access, ty = Ty.INT, assignable = false}))

        val _ = loopLevel := (!loopLevel) + 1
        val _ = breakNum := 0
        val breakLabel = Temp.newlabel()
        val body_exp = transExp(venv', tenv, body, level, breakLabel)
        val _ = loopLevel := (!loopLevel) - 1
      in
        checkInt(lo_exp, pos, "Low side of the ForExp with variable: " ^ S.name for_sym);
        checkInt(hi_exp, pos, "High side of the ForExp with variable: " ^ S.name for_sym);
        checkNoValue(body_exp, pos, " ForExp body should be UNIT"); (* ensure id not re-assigned in the body scope *)
        {exp=R.forExp(for_access, #exp lo_exp, #exp hi_exp, #exp body_exp, breakLabel), ty=Ty.UNIT}
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
        val result = case entry of SOME (E.FunEntry{result=result, ...}) => result
                                 | NONE => Ty.NIL
        val arg_exps = map trexp args
      in
          case entry of
               SOME (E.FunEntry{level=funLevel, label=label, formals=formals, result=result}) =>
               (checkFuncParams(S.name func, formals, map #ty arg_exps, pos);
                {
                  exp = R.call(level, funLevel, label, map #exp arg_exps),
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
        assertTypeEq(sizeTy, Ty.INT, err pos, "Array size must be INT");
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
        val {exp=vExp, ty=varTy} = transVar (venv, tenv, var, level)
        val {exp=eExp, ty=expTy} = trexp exp
      in
        (
        assertAssignable(venv, var, pos); 
        assertTypeEq(varTy, expTy, err pos, "Assignment " ^
          A.VarName var  ^ " type mismatch");
        {exp=R.assign(vExp, eExp), ty=Ty.UNIT}
        )
      end
    | trexp (A.WhileExp {test, body, pos}) =
      let
        val _ = loopLevel := !loopLevel + 1
        val breakLabel = Tp.newlabel() 
        val {exp=testExp, ty=test_ty} = transExp (venv, tenv, test, level, breakLabel)
        val {exp=bodyExp, ty=body_ty} = transExp (venv, tenv, body, level, breakLabel)
        val _ = loopLevel := !loopLevel - 1
        val _ = breakNum := 0
      in
        (
        assertTypeEq(test_ty, Ty.INT, err pos, "Invalid WHILE loop test type, INT expected");
        assertTypeEq(body_ty, Ty.UNIT,err pos, "Invalid WHILE loop body type, UNIT expected");
        {exp=(R.whileExp(testExp, bodyExp, breakLabel)), ty=Ty.UNIT}
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
      let
        val test_exp = trexp test
        val then_exp = trexp then'
        val _ = checkInt(test_exp, pos, "Invalid TEST expression type, INT expected")
      in
      (case else_opt of
        NONE =>(
          checkNoValue(then_exp, pos, "Invalid THEN expression type, UNIT expected");
          {exp=R.ifExp(#exp test_exp, #exp then_exp), ty=Ty.UNIT})
      | SOME(else') =>
        let
          val else_exp = trexp else'
        in
          assertTypeEq(#ty then_exp, #ty else_exp, err pos, "IfExp THEN-ELSE type mismatch");
          {exp=R.ifelseExp(#exp test_exp, #exp then_exp, #exp else_exp), ty=(#ty then_exp)}
        end
      )
      end
    | trexp (A.RecordExp {fields = fields, typ = typ, pos = pos}) =
      let
        type field = {sym:A.symbol, expty:expty, pos:A.pos}
        val recordTy = lookActualType(tenv, typ, pos)
        val ini_fields = map (fn (sym, exp, pos) =>
                              {sym=sym, expty=(trexp exp), pos=pos}) fields 
        val ini_exps = map #exp (map #expty ini_fields) 
      in
      (case recordTy of
          Types.RECORD(def_fields, unique) =>
          let
            fun check (ini_field :: ini_tail, def_field::def_tail) =
            let
              val {sym=ini_id, expty={exp=_, ty=ini_ty}, pos=pos} = ini_field 
              val (def_id, def_ty) = def_field
            in
              if compareAnyType(ini_ty, def_ty) andalso ini_id = def_id then
                check (ini_tail, def_tail)
              else
                (err pos ("Inconsistent record type: " ^ (S.name typ) ^ 
                "\nDefined field: "
                ^ (S.name def_id) ^ ":" ^ (Ty.name def_ty) ^ "\nGiven field:  " ^
                (S.name ini_id) ^ ":" ^ (Ty.name ini_ty));
                err pos (Bool.toString(compareAnyType(ini_ty, def_ty)));
                err pos (Bool.toString(ini_id = def_id));
                false)
            end
              | check ([], [_]) = (err pos ("Need more fileds for the record: "
              ^ S.name typ); false)
              | check ([_], []) = (err pos ("Too many fileds for the record: "
              ^ S.name typ); false)
              | check ([], []) = true
          in
            if check(ini_fields, def_fields) then
              {exp=R.createRecord(ini_exps), ty=lookType(tenv, typ, pos)}
            else
              {exp=R.dummy_exp, ty=Ty.UNIT}
          end
          | _ => (err pos ("Invalid RECORD type: " ^ S.name typ);
          {exp=R.dummy_exp, ty=Ty.UNIT}))
      end
    in
      trexp exp
    end

    and transDec (venv:venv, tenv:tenv, dec, level, breakLabel) =
      let fun trdec (A.VarDec{name, escape, typ, init, pos, ...}) =
       let val {exp, ty} = transExp(venv, tenv, init, level, breakLabel)
         val msgTmpl = "VarDec: "
         (* translate *)
         val access = R.allocLocal level (!escape)
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
          | NONE => assertEq(actual_ty ty, Ty.NIL, op <>, err pos, 
            msgTmpl ^ S.name name ^ " cannot be assigned to nil implicitly");
            {
             venv = S.enter(venv, name, E.VarEntry{access = access, ty = ty, assignable = true}),
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
              SOME ty => (err pos (msgTmpl ^ "same mutually recursive type name"); acc_temp)
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
         fun trParams (params: A.field list) pos=
           {
            nameTys = map (fn ({typ, name, ...}) => (name, lookType(tenv, typ, pos))) params,
            escapes = map (fn ({escape, ...}) => !escape) params
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
                     val {nameTys, escapes, ...} = trParams params pos
                     val newLabel = Tp.newlabel()
                     val entry = E.FunEntry{
                         level = R.newLevel{parent = level, name = newLabel, escapes = escapes},
                         label = newLabel,
                         formals = nameTys,
                         result = resultTy
                       }
                   in
                     (
                      S.enter(acc, name, entry),
                      case S.look(accTemp, name) of
                        SOME _ => (err pos
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
             val {nameTys, escapes, ...} = trParams params pos
             fun enterParam (((name, ty), escape), venv:venv) =
              let
               val access = R.allocLocal level escape
              in
               S.enter(venv, name, E.VarEntry{access = access, ty
               = ty, assignable = true})
              end
             val venv' = foldl enterParam venv (ListPair.zip
             (nameTys, escapes))
             val {exp = bodyExp, ty = bodyTy} = transExp(venv', tenv, body, level, Temp.newlabel())
           in
             assertTypeEq(bodyTy, resultTy, err pos, msgTmpl ^
             (S.name name) ^ " type mismatch.\nReturn type: " ^
             (Ty.name resultTy) ^ "\nBody type: " ^ (Ty.name
             bodyTy))
            ;
             (case lookupFunEntry(venv, name, pos) of
                (SOME (E.FunEntry ({level, ...}))) => R.procEntryExit(bodyExp, level))
           end
       in
         map (fn dec => trFunDecBody(venv', dec)) fundecs;
         {
          venv = venv',
          tenv = tenv ,
          exps = []
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
        let val mainlevel =
          R.newLevel {parent = R.outermost, name = Temp.namedlabel "tiger_main", escapes = []}
          val {exp=exp, ty=ty} = transExp(E.base_venv, E.base_tenv, exp, mainlevel, Tp.newlabel());
        in
          R.procEntryExit (exp, mainlevel);
          R.getResult()
        end
end
