use "env.sml";
use "types.sml";
use "symbol.sml";
use "absyn.sml";
use "transalte.sml";
use "errormsg.sml";

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

  fun assertEq (lhs: 'a, rhs: 'a, eqFun, errCurry, msg) =
      if eqFun(lhs, rhs) then
          errCurry msg
      else
          ()

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
             val lhs_ty_opt = whatis(lhs)
             val rhs_ty_opt = whatis(rhs)
           in
             case (lhs_ty_opt, rhs_ty_opt) of
                  (SOME(lhs_ty), SOME(rhs_ty)) => compareAnyType(lhs_ty, rhs_ty)
                | (_, _) => false
           end
       | (_, _) => false 

  fun assertTypeEq (lhs: Ty.ty, rhs: Ty.ty, errCurry, msg) =
    if compareAnyType(lhs, rhs) then
      errCurry msg
    else
      ()

  fun isValidRecord T.RECORD(symTys, _) =
      let 
          fun f hd::tl =
              if tl = nil then true
              else
                  let val hd2::tl2 = tl
                  in
                      hd <> hd2 andalso f tl
                  end
            | f nil = true
          val syms = ListMergeSort.sort op > (map #1 symTys)
      in
          f syms
      end
    | isValidRecord _ = false

  (* use T.NIL as dummy return value *)
  fun lookActualType(tenv, symbol, pos) =
      case S.look(tenv, symbol) of
          SOME ty => actual_ty(ty, pos)
        | NONE => (err pos ("Type " ^ S.name symbol ^ "not found"); T.NIL)

  fun actual_ty(ty) =
      case ty of
          T.NAME(s, ref(SOME(t))) => actual_ty(t)
        | T.NAME(s, ref(NONE)) => (Err.impossible ("Type " ^ S.name s ^ "is NONE"); T.NIL)
        | _ => ty

  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  fun checkInt ({exp, ty}, pos) = 
    assertEq (ty, Ty.INT, op =, err pos, "integer required")
  
  fun checkNoValue ({exp, ty}, pos) =
    assertEq (ty, Ty.UNIT, op =, err pos, "no-value required")
    
  (* use E.FunEntry {...} as dummy return value *)
  (* If error, exit ? or return dummy entry ? *)
  fun lookupFunEntry (venv, func:S.symbol, pos) = 
    case S.look(venv, func) of
         SOME fun_entry => fun_entry
       | NONE => (err pos "Function " ^ S.name func ^ "not found"; E.FunEntry {formals = [], result = Ty.UNIT})

  fun checkFuncParams (formals: E.ty list, args: A.exp list, pos) = 
  let
    fun f (lhs_head::lhs_tail, rhs_head::rhs_tail) =
        (
        assertTypeEq(lhs_head, rhs_head, err pos,
        "Parameter Mismatch:\n" ^ "function parameter: " ^ Ty.name lhs_head ^
        "input parameter: " ^ Ty.name rhs_head);
        f (lhs_tail, rhs_tail)
        )
      | f ([], []) = () 
      | f (_, _) = err pos "Parameter size mismatch"
  in
    f(formals, args)
  end

  fun transExp(venv, tenv, exp) =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
        (
        checkInt(trexp left, pos);
        checkInt(trexp right, pos);
        {exp=(), ty=Ty.INT}
        )
      | trexp (A.IntExp int) =
        {exp=(), ty=Ty.INT}
      | trexp (A.LetExp {decs, body, pos}) =
        let
          val {venv=venv', tenv=tenv'} =
          transDec(venv, tenv, decs)
        in
          transExp(venv', tenv', body)
        end
      | trexp (A.SeqExp seq) =
        case seq of
             [] => err ~1 "two or more expression in seq requried"
           | [(exp, pos)] => trexp(exp)
           | (exp, pos)::tail =>
               (
               trexp(exp);
               trexp(tail)
               )
      | trexp (A.ForExp {id, escape, lo, hi, body, pos}) =
        (
        checkInt(trexp lo, pos);
        checkInt(trexp hi, pos);
        let
          val venv' = S.enter(venv, id, E.VarEntry{ty = Ty.UNIT})
        in
          checkNoValue(transExp(venv', tenv, body)) (* ensure id not re-assigned in the body scope *)
        end;
        {exp = (), ty=Ty.UNIT}
        )
      | trexp (A.VarExp var) =
        transVar(venv, tenv, var)
      | trexp (A.NilExp) =
        {exp=(), ty=Ty.NIL}
      | trexp (A.StringExp) =
        {exp=(), ty=Ty.STRING}        
      | trexp (A.CallExp {func, args, pos}) =
        (
        let
          val {formals=formals, result=result} = lookupFunEntry(venv, func, pos)
          val () = checkFuncParams(formals, args, pos)
        in
          {exp=(), ty=result}
        end
        )
      | trexp (A.ArrayExp {typ, size, init, pos}) =
      	let
	  val sizeTy = trexp size
	  val initTy = trexp init
	in
	  case S.look(tenv, typ) of
	    SOME (ty) =>
	      case actual_ty(ty, pos) of
	        Ty.ARRAY(t, u) =>
		  if checkInt(sizeTy, pos) then
		    if assertTypeEq({exp=(), ty=t}, initTy, err pos, "") then
		      {exp=(), ty=Ty.ARRAY(t, u)}
		    else
		      err pos ("Array initial type does not match base type");
		      {exp=(), ty=Ty.UNIT}
		  else
		    err pos ("Array size must be INT");
		    {exp=(), ty=Ty.UNIT}
		| _ => err pos ("Return type must be an ARRAY");
		       {exp=(), ty=Ty.UNIT}
	    | _ => err pos ("Unable to define ARRAY based on current type");
	      	   {exp=(), ty=Ty.UNIT}
	end
	(* ... *)
    in
      trexp exp
    end

  fun transDec (venv, tenv, dec) =
      let fun trdec (A.VarDec{name, typ = NONE, init, pos}) =
              let val {exp, ty} = transExp(venv, tenv, init)
              in
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = ty}),
                    tenv = tenv
                  }
              end
            | trdec (A.VarDec{name, typ = SOME(s, _), init, pos}) = 
              let val {exp, ty} = transExp(venv, tenv, init)
                  val msgTmpl = "VarDec: "
                  val decTy = lookActualType(tenv, s, pos)
              in
                  case ty of
                      T.NIL => assertEq(isValidRecord decTy, true, op =, err pos, msgTmpl ^ "Invalid record")
                    | T.RECORD =>
                      (
                        assertEq(isValidRecord ty, true, op =, err pos, msgTmpl ^ "Invalid record");
                        assertEq(isValidRecord decTy, true, op =, err pos, msgTmpl ^ "Invalid record");
                        (* structural equal *)
                        assertEq(#1 ty, #1 decTy, op =, err pos, msgTmpl ^ "record type mismatch")
                      )
                    | T.ARRAY =>
                      (* structural equal *)
                      assertEq(#1 ty, #1 decTy, op =, err pos, msgTmpl ^ "array type mismatch")
                    | _ => assertEq(decTy, ty, op =, err pos, msgTmpl ^ "type mismatch - " ^ (S.name s))
                ;
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = decTy}),
                    tenv = tenv
                  }
              end
            | trdec (A.TypeDec(tydecs)) =
              let
                  (* first pass to scan headers*)
                  fun trTyDecHeader (tenv, {name}::tl) =
                      let val tenv' = S.enter(tenv, name, Types.NAME(name, ref NONE))
                      in
                          trTyDecHeader(tenv', tl)
                      end
                    | trTyDecHeader (tenv, nil) = tenv
                  (* second pass to fill body *)

                  fun trTyDecBody (tenv, {name, ty, pos}::tl) =
                      let val nameTy = transTy(tenv, ty)
                      in
                          nameTyRef := lookActualType(tenv, name, pos);
                          trTyDecBody(tenv, tl)
                      end
                    | trTyDecBody (tenv, nil) = tenv
                  val tenv' = trTyDecHeader(tenv, tydecs)
              in
                  trTyDecBody(tenv', tydecs)
              end
      in
          trdec dec
      end
end
