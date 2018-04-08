structure Liveness: LIVENESS = struct

  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet FlowGraph.Table.table

  structure IGraph = Flow.Graph
  structure LiveGraph = Flow.Graph

  fun flow2liveGraph (flow: Flow.flowgraph) =
  let
    fun transFlow2Live = 
    let
      (* Firstly, copy the original flow graph *)
      (* Only nodes copied, edges remain uncopied *)
      fun init (flownode, livegraph) =
      let
        val (nid, attrs, _, _) = flownode;
        val {def=def, use=use, move=move} = attrs;
        val new_attrs = {def=def, use=use, move=move, li=[], lo=[]};
      in
        LiveGraph.addNode(livegraph, nid, new_attrs);
      end

      val initgraph = LiveGraph.foldNodes init LiveGraph.empty flow

      (* Second, copy the edges *)
      (* lg: livegraph *)
      fun trans (lg_old_node, lg_new) =
      let
        val (nid, old_attrs, old_succs, old_preds) = lg_old_node;
        val lg_new_node = LiveGraph.getNode(lg_new, nid);
        val (_, new_attrs, _, _) = lg_new_node;
        fun remap node_set = LiveGraph.NodeSet.map (fn (nid, _, _, _) =>
          LiveGraph.getNode(lg_new, nid)) node_set;
        val new_succs = remap old_succs;
        val new_preds = remap old_preds;
      in
        LiveGraph.NodeMap.insert(lg_new, nid, (nid, new_attrs, new_succs,
        new_preds));
      end

      val transgraph = LiveGraph.foldNodes trans flow initgraph

      (* Third, recursively calculate the liveness *)
      fun liveiterate = 
      let
      in
      end

    in

    end

    fun iterateLive = 
    let
    in
    end
  in
  end
  
  struct TSet = Temp.Set
  struct TMap = Temp.Map
  
  fun interferenceGraph (flow: Flow.flowgraph) = 
    let
      val lGraph = flow2liveGarph(flow)
      val (iGraph, tMap, mEdges) = LGraph2IGraph lGraph
      fun tnode x = Flow.Graph.getNode (iGraph, lookNid tMap x)
      fun gtemp x = Flow.Graph.nodeInfo(x)
      val mEdges =
        let
          fun isSame({from=f1, to=t1}, {from=f2, to=t2}) =
            if (f1=f2 andalso t1=t2) oralso (f1=t2 andalso f2=t1) then true else false
          fun hasEdge(e, []) = false
            | hasEdge(e, h::l) = if isSame(e, h) then true else hasEdge(e, l)
        in
          foldl (fn (e, eList) => if hasEdge(e, eList) then eList else e::eList) [] mEdges
        end
    in
      IGRAPH{
        graph=iGraph
        tnode=tnode
        gtemp=gtemp
        moves=mEdges
      }
    end
  and fun LGraph2IGraph lgraph =
    let
    in
    end
  
  exception NidNotFound
  fun lookNid tMap x =
    case TMap.find (tMap, x) of
      SOME(nid) => nid
      | _ => raise NidNotFound
    
end
