structure Liveness: LIVENESS = struct

  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet FlowGraph.Table.table

  structure IGraph = Flow.Graph
  structure LiveGraph = Flow.Graph

  fun flow2liveGraph (flow: Flow.flowgraph) =
  let
    fun transFlow2Live = 
    let
    in
    end

    fun iterateLive = 
    let
    in
    end
  in
  end

  fun interferenceGraph (flow: Flow.flowgraph) = 
  let
    val livegraph = flow2liveGarph(flow)

    fun transLive2Inter =
    let
    in
    end
  in
  end
    
end
