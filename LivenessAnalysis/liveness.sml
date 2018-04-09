structure Liveness: LIVENESS = struct

  structure Graph = Flow.Graph
  structure FGraph = Flow.Graph
  structure IGraph = Flow.Graph
  structure LiveGraph = Flow.Graph
  structure TSet = Temp.Set
  structure TMap = Temp.Map


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
  datatype live = LIVE of {def: TSet.set,
                           use: TSet.set,
                           move: (Temp.temp * Temp.temp) option,
                           li: TSet.set ref,
                           lo: TSet.set ref}

  fun list2set lst =
    foldl (fn (item, set) => TSet.add(set, item)) TSet.empty lst

  fun flow2liveGraph (flow: Flow.flowgraph) =
  let
    type t_fnode = Flow.t_node FGraph.node
    fun transFlow2Live () = 
    let
      (* Firstly, copy the original flow graph *)
      (* Only nodes copied, edges remain uncopied *)
      fun init (flownode: t_fnode, livegraph) =
      let
        val nid = FGraph.getNodeID(flownode)
        val attrs = FGraph.nodeInfo(flownode)
        val {def=def, use=use, move=move} = attrs
        val def = list2set def
        val use = list2set use
        val new_attrs = {def=def, use=use, move=move, li=TSet.empty,
        lo=TSet.empty}
      in
        LiveGraph.addNode(livegraph, nid, new_attrs)
      end

      val initgraph = LiveGraph.foldNodes init LiveGraph.empty flow

      (*
      (* Second, copy the edges *)
      (* lg: livegraph *)
      fun trans (lg_old_node, lg_new) =
      let
        val nid = LiveGraph.getNodeID(lg_old_node)
        val old_attrs = LiveGraph.nodeInfo(lg_old_node)
        val old_succs = LiveGraph.succs(lg_old_node)
        val old_preds = LiveGraph.preds(lg_old_node)
        val lg_new_node = LiveGraph.getNode(lg_new, nid)
        val new_attrs = LiveGraph.nodeInfo(lg_new_node)
        fun remap node_set = LiveGraph.NodeSet.map (fn nid =>
          LiveGraph.getNode(lg_new, nid)) node_set
        val new_succs = remap old_succs
        val new_preds = remap old_preds
      in
        LiveGraph.NodeMap.insert(lg_new, nid, (nid, new_attrs, new_succs,
        new_preds))
      end

      val transgraph = LiveGraph.foldNodes trans flow initgraph
      *)
      val transgraph = initgraph

      (* Third, recursively calculate the liveness *)
      fun liveiterate () = 
      let
        fun update nid graph =
        let
          val node = LiveGraph.getNode(graph, nid)
          val attrs = LiveGraph.nodeInfo(node)
          val succs = LiveGraph.succs(node)
          val preds = LiveGraph.preds(node)
          val {def=def, use=use, move=move, li=old_li, lo=old_lo} = attrs
          val new_li = TSet.union(use, (TSet.difference(old_lo, def)))
          val new_lo = foldl (
            fn (nid, set) => 
            let
              val n = LiveGraph.getNode(graph, nid)
              val li = #li (LiveGraph.nodeInfo node)
            in
              TSet.union(set, li)
            end
          ) TSet.empty succs
          val is_stable = ((TSet.compare(old_lo, new_lo) = EQUAL) andalso 
                           (TSet.compare(old_li, new_li) = EQUAL))
          val d = {def=def, use=use, move=move, li=new_li, lo=new_lo}
          val new_graph = LiveGraph.changeNodeData(graph, nid, d) 
        in
          (is_stable, new_graph)
        end

        fun helper (false, graph) =
        let
          val iterated = 
          LiveGraph.foldNodes (fn (node, (is_stable, graph)) =>
          let
            val nid = LiveGraph.getNodeID(node)
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

  exception NidNotFound
  fun interferenceGraph (flow: Flow.flowgraph) = 
    let
      val lgraph = flow2liveGraph(flow)
      val (igraph, tmap, moves ) = LGraph2IGraph lgraph
      fun tnode x = Flow.Graph.getNode (igraph, lookNid (tmap x))
      fun gtemp x = Flow.Graph.nodeInfo(x)
      val moves =
        let
          fun isSame({from=f1, to=t1}, {from=f2, to=t2}) =
            if (f1=f2 andalso t1=t2) orelse (f1=t2 andalso f2=t1) then true else false
          fun hasEdge(e, []) = false
            | hasEdge(e, h::l) = if isSame(e, h) then true else hasEdge(e, l)
        in
          foldl (fn (e, eList) => if hasEdge(e, eList) then eList else e::eList) [] moves
        end
    in
      IGRAPH{graph=igraph, tnode=tnode, gtemp=gtemp, moves=moves}
    end
  and LGraph2IGraph lgraph =
    let
      val count = ref 0
      val tmap = TMap.empty
      val igraph = IGraph.empty
      fun insertTemp (temp, (igraph, tmap)) =
        case TMap.find (tmap, temp) of
          SOME(i) =>
            (igraph, tmap)
          | NONE =>
            let
              val nid = !counter
              val () = counter := nid + 1
            in
              (Graph.addNode (igraph, nid, temp), TMap.insert (tmap, temp, nid))
            end
      (* insert temps *)
      val (igraph, tmap) = Graph.foldNodes (
        fn (lnode, (igraph, tmap)) =>
          let
            val LIVE {def, use, move, li,lo} = Graph.nodeInfo lnode
            val (igraph, tmap) = TSet.foldl insertTemp (igraph, tmap) def
            val (igraph, tmap) = TSet.foldl insertTemp (igraph, tmap) use
          in
            (igraph, tmap)
          end
        ) (igraph, tmap) lgraph
      (* insert edges *)
      fun insertEdges (lnode, (igraph, moves)) =
        let
          val LIVE {def, use, move, li, lo} = Graph.nodeInfo lnode
          val (igraph, moves) = TSet.foldl (
            fn (defitem, (igraph, moves)) => (TSet.foldl (
              fn (outitem, (igraph, moves)) => case move of
                SOME(move) =>
                  let
                    val (useitem::_) = TSet.listItems useset
                    val fromid = lookNid tmap useitem
                    val toid = lookNid tmap defitem
                    val outid = lookNid tmap outitem
                  in
                    if (fromid = outid) then (igraph, {from=fromid, to=toid}::moves)
                    else (Graph.doubleEdge (igraph, toid, outid), moves)
                  end
                | None =>
                  (Graph.doubleEdge (igraph, lookNid tmap defitem, lookNid tmap outitem), moves)
              ) (igraph, moves) (!lo)
            )
          ) (igraph, moves) def
        in
          (igraph, moves)
        end
      (* add edges *)
      val (igraph, moves) = Graph.foldNodes insertEdges (igraph, []) lgraph
    in
      (igraph, tmap, moves)
    end
  and lookNid (tmap x) =
    case TMap.find (tmap, x) of
      SOME(nid) => nid
      | _ => raise NidNotFound
  
  fun ts2s set = tSet.foldl (fn (item,
     s) => s ^ " " ^ (Int.toString item)) "" set
  fun showlive livegraph = (
     print("Live Graph\n");
     Graph.printGraph (fn (nid, {def=def, use=use, move=move,
     li=li, lo=lo}:t_lnode) => (Int.toString nid) ^ 
     "\nlive out: " ^ (ts2s lo) ^
     "\nlive in: " ^ (ts2s li)) livegraph)
end
    

