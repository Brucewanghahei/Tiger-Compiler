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
  structure S = Symbol
  structure T = Types
  structure R = Translate

  type access = unit (* todo: unknown *)
  type ty = T.ty
  datatype enventry = VarEntry of {access:Translate.access, ty: ty, assignable: bool}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label,
                                   formals: (S.symbol * ty) list, result: ty}

  type tenv = ty S.table       (* predefined types *)
  type venv = enventry S.table (* predefined functions *)

  val base_tenv = let
      val primitiveTypes = [("int", T.INT), ("string", T.STRING), ("unit", T.UNIT)]
  in
      foldl (fn ((name, ty), acc) => S.enter(acc, S.symbol name, ty)) S.empty primitiveTypes
  end
  val base_venv =
      let
          val dummySymbol = S.symbol ""
          fun ty2formal ty = (dummySymbol, ty)
          fun s name = S.symbol name 
          val predefinedVars = 
              [("nil", VarEntry {access=(R.allocLocal R.outermost true), ty=T.NIL, assignable = false})
              ,("print", FunEntry {level=R.outermost, label=s "tig_print", formals= map ty2formal [T.STRING], result=T.UNIT})
              ,("flush", FunEntry {level=R.outermost, label=s "tig_flush", formals=[], result=T.UNIT})
              ,("getchar", FunEntry {level=R.outermost, label=s "tig_getchar", formals=[], result=T.STRING})
              ,("ord", FunEntry {level=R.outermost, label=s "tig_ord", formals=map ty2formal [T.STRING], result=T.INT})
              ,("chr", FunEntry {level=R.outermost, label=s "tig_chr", formals=map ty2formal [T.INT], result=T.STRING})
              ,("size", FunEntry {level=R.outermost, label=s "tig_size", formals=map ty2formal [T.STRING], result=T.INT})
              ,("substring", FunEntry {level=R.outermost, label=s "tig_substring", formals=map ty2formal [T.STRING, T.INT, T.INT], result=T.STRING})
              ,("concat", FunEntry {level=R.outermost, label=s "tig_concat", formals=map ty2formal [T.STRING, T.STRING], result=T.STRING})
              ,("not", FunEntry {level=R.outermost, label=s "tig_not", formals=map ty2formal [T.INT], result=T.INT})
              ,("exit", FunEntry {level=R.outermost, label=s "tig_exit", formals=map ty2formal [T.INT], result=T.UNIT})
              ]
      in
          foldl (fn ((name, entry), acc) => S.enter(acc, S.symbol name, entry)) S.empty predefinedVars
      end
end
