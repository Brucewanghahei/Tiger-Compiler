structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of Symbol.symbol * ty option ref
          | UNIT

  fun whatis(NAME (sym, ref(ty_opt))) =
    (
    case ty_opt of
         SOME(ty) => whatis(ty)
       | NONE => NONE
    )
  | whatis(other_ty) = SOME(other_ty)


  fun name
    (RECORD (symbol_ty_list, uni)) =
    let
      fun map_symbolTy_to_string (symbol, ty) = 
        (Symbol.name symbol) ^ ":" ^ (name ty)
      val body = foldl(fn (a,b) => b ^ ", " ^  map_symbolTy_to_string(a))("")(symbol_ty_list)
    in
      "Record: {" ^ String.extract(body, 2, NONE) ^ "}"
    end
  | name NIL = "Nil"
  | name INT = "Integer"
  | name STRING = "String"
  | name (ARRAY (ty, uni)) = "Array of " ^ (name ty)
  | name (NAME (sym, ty_opt_ref)) =
      (
      case ty_opt_ref of 
        ref(SOME(ty)) => (Symbol.name sym) ^ " (" ^ (name ty) ^ ")"
      | ref(NONE) => (Symbol.name sym)
      )
  | name UNIT = "Unit"
end

