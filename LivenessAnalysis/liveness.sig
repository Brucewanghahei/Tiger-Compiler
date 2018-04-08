signature LIVENESS =
sig
  structure LiveGraph : FUNCGRAPH
  structure IGraph : FUNCGRAPH
  structure Graph: FUNCGRAPH

  type t_tset = Temp.Set.set
  type t_lnode = {def: t_tset, use: t_tset, 
                  move: (Temp.temp * Temp.temp) option,
                  li: t_tset, lo: t_tset}
  type t_inode = Temp.temp Graph.node
  type t_igraph = Temp.temp Graph.graph

  type t_lgraph = t_lnode Graph.graph
  
  datatype igraph = IGRAPH of {graph: t_igraph,
                               tnode: Temp.temp -> t_inode,
                               gtemp: t_inode -> Temp.temp,
                              moves: (t_inode * t_inode) list}

  val interferenceGraph : Flow.flowgraph -> t_igraph * (Flow.t_node -> Temp.temp list)
  val flow2liveGraph : Flow.flowgraph -> t_lgraph

  val show : TextIO.outstream * igraph -> unit
end
