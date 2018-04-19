signature COLORING =
sig
    structure G: FUNCGRAPH
    structure L: LIVENESS
    type t_cnode = {tmp: Temp.temp, color: int}
    type cGraph = t_cnode G.graph
    val color: Assem.instr list * L.igraph -> cGraph
    val simplify: L.igraph -> bool * L.igraph * Temp.temp list
    val coalesce: L.igraph -> bool * L.igraph * Temp.temp list
    val freeze: L.igraph -> bool * L.igraph * Temp.temp list
    val potentialSpill: L.igraph -> bool * L.igraph * Temp.temp list
    val select: L.igraph -> bool * L.igraph * Temp.temp list
    val actualSpill: L.igraph -> bool * L.igraph * Temp.temp list
end
