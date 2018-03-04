use "env.sml";
use "types.sml";
use "symbol.sml";
use "absyn.sml";
use "transalte.sml";

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

  structure A = Absyn
  structure Ty = Types

  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  fun checkInt ({exp, ty}, pos) =
    case ty of Ty.INT => ()
       | _ => error pos "integer required";

  fun transExp(venv, tenv, exp) =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
        (checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp=(), ty=Ty.INT})
        (* ... *)
    in
      trexp exp
    end
end
