use "env.sml";
use "types.sml";
use "symbol.sml";
use "absyn.sml";
use "transalte.sml";

structure Semant =
struct
  
  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty} 

  fun transVar(venv, tenv, var:Absyn.var) : expty = ()

  fun transExp(venv, tenv, exp:Absyn.exp) : expty = ()

  fun transDec(venv, tenv, dec:Absyn.dec) : {venv: venv, tenv: tenv} = ()

  fun transTy(tenv, ty:Absyn.ty) : Types.ty

  fun transProg(exp:Absyn.exp) : unit = ()

end
