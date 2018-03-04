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

  fun errWrapper pos msg =
      fn () => err pos msg

  fun assertEq (lhs: 'a, rhs: 'a, eqFun, errFun: unit -> unit) =
      if eqFun(lhs, rhs) then
          errFun()
      else
          ()

  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  fun checkInt ({exp, ty}, pos) =
    case ty of Ty.INT => ()
       | _ => Err.error pos "integer required";

  fun transExp(venv, tenv, exp) =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
        (checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp=(), ty=Ty.INT})
      | trexp (A.IntExp int) =
        {exp=(), ty=Ty.INT}
      | trexp (A.LetExp {decs, body, pos}) =
        let
          val {venv=venv', tenv=tenv'} =
          transDec(venv, tenv, decs)
        in
          transExp(venv', tenv', body)
        end
        (* ... *)
    in
      trexp exp
    end

  fun transDec (venv, tenv, dec) =
      let fun trdec (A.VarDec{name, typ, init, pos}) =
              let val {exp, ty} = transExp(venv, tenv, init)
              in
                  case typ of
                      SOME(decTy, pos) =>
                      let val errFun = errWrapper pos "VarDec: type mismatched"
                      in
                          case ty of
                              T.NIL => assertEq(decTy, T.RECORD)
                          (* todo: how to use unique; handle symbol in T.NAME*)
                           | T.RECORD(fields, unique) => 
                           | T.ARRAY(eleTy, unique) =>
                           | T.NAME(symbol, refTy) => 
                           | _ => assertEq(decTy, ty, op =, errFun)
                      end
                    | NONE => ()
                ;
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = ty}),
                    tenv = tenv
                  }
              end
      in
          trdec dec
      end
end
