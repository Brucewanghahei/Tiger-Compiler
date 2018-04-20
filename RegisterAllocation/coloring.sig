signature COLORING =
sig
    structure G: FUNCGRAPH
    structure L: LIVENESS
    structure F: FRAME
    type allocation = Frame.register Temp.map
    type t_cnode = {tmp: Temp.temp, color: int}
    type cGraph = t_cnode G.graph
    val color: Assem.instr list * int -> Assem.instr list * allocation
end
