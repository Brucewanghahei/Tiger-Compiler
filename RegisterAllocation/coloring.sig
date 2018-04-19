signature COLORING =
sig
    structure G: FUNCGRAPH
    structure L: LIVENESS
    type t_cnode = {tmp: Temp.temp, color: int}
    type cGraph = t_cnode G.graph
    val color: Assem.instr list * L.igraph * int -> cGraph
end
