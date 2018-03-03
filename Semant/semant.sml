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
   type venv = Env.enventry Symbol.table
   type tenv = ty Symbol.table
   type expty = {exp: Translate.exp, ty: Types.ty}

end
