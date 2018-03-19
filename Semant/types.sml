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


  structure StringSet = SplaySetFn(struct type ord_key = string
      val compare = String.compare
    end)
  
  fun name ty = 
  let
    val name_set = ref StringSet.empty
    fun name_helper
      (RECORD (symbol_ty_list, uni)) =
      let
        fun map_symbolTy_to_string (symbol, ty) = 
          (Symbol.name symbol) ^ ":" ^ (name_helper ty)
        val body = foldl(fn (a,b) => b ^ ", " ^  map_symbolTy_to_string(a))("")(symbol_ty_list)
      in
        "Record: {" ^ String.extract(body, 2, NONE) ^ "}"
      end
    | name_helper NIL = "Nil"
    | name_helper INT = "Integer"
    | name_helper STRING = "String"
    | name_helper (ARRAY (ty, uni)) = "Array of " ^ (name_helper ty)
    | name_helper UNIT = "Unit"
    | name_helper (NAME (sym, ty_opt_ref)) =
    (
        case (StringSet.member (! name_set, Symbol.name sym)) of
           true => Symbol.name sym  
         | false => (name_set := StringSet.add ((!name_set), (Symbol.name sym));
                  case ty_opt_ref of 
                    ref(SOME(ty)) => (Symbol.name sym) ^ " (" ^ (name_helper ty) ^ ")"
                  | ref(NONE) => (Symbol.name sym)
                  )
    )
  in
    name_helper ty 
  end
end
