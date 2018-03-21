signature TRANSLATE =
sig
    type exp
    type level
    type access (* not the same as Frame.access *)

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    escapes: bool list} -> level

    val formals : level -> access list
    val allocLocal : level -> bool -> access
end
