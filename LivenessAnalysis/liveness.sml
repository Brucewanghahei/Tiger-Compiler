structure Liveness: LIVENESS = struct

  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet FlowGraph.Table.table

  structure IGraph = Flow.Graph
  structure LiveGraph = Flow.Graph
  structure tSet = Temp.Set
  structure tMap = Temp.Map

  fun list2set lst =
    foldl (fn (item, set) => tSet.add(set, item)) tSet.empty lst

  fun flow2liveGraph (flow: Flow.flowgraph) =
  let
    fun transFlow2Live () = 
    let
      (* Firstly, copy the original flow graph *)
      (* Only nodes copied, edges remain uncopied *)
      fun init (flownode, livegraph) =
      let
        val (nid, attrs, _, _) = flownode
        val {def=def, use=use, move=move} = attrs
        val def = list2set def
        val use = list2set use
        val new_attrs = {def=def, use=use, move=move, li=tSet.empty,
        lo=tSet.empty}
      in
        LiveGraph.addNode(livegraph, nid, new_attrs)
      end

      val initgraph = LiveGraph.foldNodes init LiveGraph.empty flow

      (* Second, copy the edges *)
      (* lg: livegraph *)
      fun trans (lg_old_node, lg_new) =
      let
        val (nid, old_attrs, old_succs, old_preds) = lg_old_node
        val lg_new_node = LiveGraph.getNode(lg_new, nid)
        val (_, new_attrs, _, _) = lg_new_node
        fun remap node_set = LiveGraph.NodeSet.map (fn (nid, _, _, _) =>
          LiveGraph.getNode(lg_new, nid)) node_set
        val new_succs = remap old_succs
        val new_preds = remap old_preds
      in
        LiveGraph.NodeMap.insert(lg_new, nid, (nid, new_attrs, new_succs,
        new_preds))
      =end

      val transgraph = LiveGraph.foldNodes trans flow initgraph

      (* Third, recursively calculate the liveness *)
      fun liveiterate () = 
      let
        fun update nid graph =
        let
          val (nid, attrs, succs, preds) = LiveGraph.getNode(graph, nid)
          val {def=def, use=use, move=move, li=old_li, lo=old_lo} = attrs
          val new_li = tSet.union(use, (tSet.difference(old_lo, def)))
          val new_lo = foldl (
            fn (nid, set) => 
            let
              val (_, {def=_, use=_, move=_, li=li, lo=_}, _, _) =
                LiveGraph.getNode(graph, nid);
            in
              tSet.union(set, li)
            end
          ) tSet.empty succs
          val is_stable = ((tSet.compare(old_lo, new_lo) = EQUAL) andalso 
                           (tSet.compare(old_in, new_in) = EQUAL))
          val new_graph = LiveGraph.addNode(graph,
            (nid, {def=def, use=use, move=move, li=new_li, lo=new_lo}, succs,
            preds))
        in
          (is_stable, new_graph)
        end

        fun helper (false, graph) =
        let
          val iterated = 
          LiveGraph.foldNodes (fn (node, (is_stable, graph)) =>
          let
            val (nid, _, _, _) = node
            val (new_stable, new_graph) = update nid graph
          in
            (is_stable andalso new_stable, new_graph)
          end
          ) (true, graph) graph
        in
          helper iterated
        end
          | helper (true, graph) = graph
      in
        helper (false, transgraph)
      end
    in
      liveiterate()
    end
  in
    transFlow2Live()
  end
  
  fun interferenceGraph (flow: Flow.flowgraph) = 
    let
      val lGraph = flow2liveGarph(flow)
      val (iGraph, tMap, mEdges) = LGraph2IGraph lGraph
      fun tnode x = Flow.Graph.getNode (iGraph, lookNid tMap x)
      fun gtemp x = Flow.Graph.nodeInfo(x)
      val mEdges =
        let
          fun isSame({from=f1, to=t1}, {from=f2, to=t2}) =
            if (f1=f2 andalso t1=t2) orelse (f1=t2 andalso f2=t1) then true else false
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
  and LGOAraph2IGraph lgraph =
    let
    in
      ()
    end

  exception NidNotFound
  fun lookNid tMap x =
    case TMap.find (tMap, x) of
      SOME(nid) => nid
      | _ => raise NidNotFound
    
end
    

