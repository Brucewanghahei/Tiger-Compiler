signature LIVENESS =
sig
  datatype igraph = IGRAPH of {IGraph.graph,
                               tnode: Temp.temp -> IGraph.node,
                               gtemp: IGraph.node -> Temp.temp,
                              moves: (IGraph.node * IGraph.node) list}

  structure LiveGraph
  structure IGraph

  val interferenceGraph : Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
  val flow2liveGraph : Flow.flowgraph -> LiveGraph.graph

  val show : outstream * igraph -> unit

end
