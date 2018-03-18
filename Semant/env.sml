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
  datatype enventry = VarEntry of {ty: ty, assignable: bool}
                    | FunEntry of {formals: (Symbol.symbol * ty) list, result: ty}

  type tenv = ty Symbol.table       (* predefined types *)
  type venv = enventry Symbol.table (* predefined functions *)

  val base_tenv = let
      val primitiveTypes = [("int", Types.INT), ("string", Types.STRING), ("unit", Types.UNIT)]
  in
      foldl (fn ((name, ty), acc) => Symbol.enter(acc, Symbol.symbol name, ty)) Symbol.empty primitiveTypes
  end
  val base_venv = Symbol.empty
end
