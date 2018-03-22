signature TRANSLATE =
sig
    type exp
    type level
    type access (* not the same as Frame.access *)

    structure Frame : FRAME
    val getResult : unit -> Frame.frag list

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    escapes: bool list} -> level

    val formals : level -> access list
    val allocLocal : level -> bool -> access

    (* construct Tree *)
    val nilkw: exp
    val intlit: int -> exp
    val strlit: string -> exp
    val simpleVar: access * level -> exp
    val assign: exp * exp -> exp
    val subVar: exp * exp -> exp
    val createArray : exp * exp -> exp
    val createRecord: exp list -> exp

    (* function declaration *)
    val procEntryExit1: exp * level -> unit

    (* utility functions *)
    (* call_level * definition_level * current_fp -> access -> IR *) 
    val trace_levels: (level * level * Temp.temp) -> Frame.access -> Tree.exp
    val dummy_exp : exp
    val seq: Tree.exp list -> Tree.exp
end
