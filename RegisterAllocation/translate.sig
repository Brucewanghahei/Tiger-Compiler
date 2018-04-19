signature TRANSLATE =
sig
    type exp
    type level
    type access (* not the same as Frame.access *)
    type frag

    structure Frame : FRAME
    val getResult : unit -> Frame.frag list
    val fragments : frag list ref

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    escapes: bool list} -> level

    val formals : level -> access list
    val allocLocal : level -> bool -> access

    (* function declaration *)
    val procEntryExit: exp * level -> unit

    (* construct Tree *)
    val nilkw: exp
    val intlit: int -> exp
    val strlit: string -> exp
    val simpleVar: access * level -> exp
    val assign: exp * exp -> exp
    val subVar: exp * exp -> exp
    val createArray : exp * exp -> exp
    val createRecord: exp list -> exp
    val letexp: exp list * exp -> exp
    val arithOp : Absyn.oper * exp * exp -> exp
    val compOp : Absyn.oper * exp * exp -> exp
    val seqexp : exp list -> exp

    (* utility functions *)
    val whileExp: exp * exp * Temp.label -> exp
    val breakExp: Tree.label -> exp
    val ifExp: exp * exp -> exp
    val ifelseExp: exp * exp * exp -> exp
    val forExp: access * exp * exp * exp * Temp.label -> exp

    (* callLevel * decLevel * args -> result exp*)
    val call: (level * level * Temp.label * exp list) -> exp
    
    val dummy_exp : exp
    val seq: Tree.stm list -> Tree.stm
end
