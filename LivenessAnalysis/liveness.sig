signature LIVENESS =
sig
  structure LiveGraph : FUNCGRAPH
  structure IGraph : FUNCGRAPH
  structure Graph: FUNCGRAPH
  structure TSet: ORD_SET

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
  datatype live = LIVE of {def: Temp.Set.set,
                           use: Temp.Set.set,
                           move: (Temp.temp * Temp.temp) option,
                           li: Temp.Set.set ref,
                           lo: Temp.Set.set ref}

  val interferenceGraph : Flow.flowgraph -> t_igraph * (Flow.t_node -> Temp.temp list)
  val flow2liveGraph : Flow.flowgraph -> t_lgraph

  val show : igraph -> unit
  val showlive: t_lgraph -> unit
end
