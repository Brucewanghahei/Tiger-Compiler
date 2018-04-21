signature COLOR =
sig
    structure G: FUNCGRAPH
    structure L: LIVENESS
    structure F: FRAME
    type allocation
    type t_cnode
    type cGraph
    val color: Assem.instr list * int -> Assem.instr list * allocation
end
