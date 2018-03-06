use "symbol.sml";
(*
signature ENV =
sig
  type access
  type ty
  type enventry
  type tenv
  type venv
  
  val base_tenv: tenv
  val base_venv: venv
end

structure Env : ENV =
*)
structure Env =
struct
  type access = unit (* todo: unknown *)
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}

  type tenv = ty Symbol.table       (* predefined types *)
  type venv = enventry Symbol.table (* predefined functions *)

  val base_tenv = Symbol.empty
  val base_venv = Symbol.empty
end
