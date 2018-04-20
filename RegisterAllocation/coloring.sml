structure Coloring =
struct

structure G = Flow.Graph
structure L = Liveness
structure F = MipsFrame

(* infix op have to be declared in every module *)
infixr 3 </ fun x </ f = f x (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)

fun extractIgraph (L.IGRAPH{graph, tnode, gtemp, moves}) =
    {graph = graph, tnode = tnode, gtemp = gtemp, moves = moves}
  | extractIgraph _ = ErrorMsg.impossible "extract IGRAPH"

fun updateIgraph L.IGRAPH{graph, tnode, gtemp, moves} graph' =
    L.IGRAPH{graph = graph',
             (* warning: tnode is not updated! Should not use if coalesce is implemented *)
             tnode = tnode,
             gtemp = gtemp,
             moves = moves >/ List.filter (fn (x, y) =>
                                              G.hasNode(graph', x)
                                              andalso G.hasNode(graph', y))
            }

fun color (instrs, k) =
    let
        fun genIGraph(instrs) = 
                    instrs >/ MakeGraph.instrs2graph
                    >/ Liveness.interferenceGraph (* return (igraph, lgraph) *)
                    >/ #1
        val origin_igrpah = genIGraph(instrs)
        fun build(igraph: L.igraph) = 
          simplify(igraph, [])
        and simplify (igraph: L.igraph, nodeStk: t_inode List) =
            let val {graph, ...} = extractIgraph igraph
                fun helper (graph, nodeStk, nodeCands) =
                    case nodeStk of
                        nil => (graph, nodeStk, nodeCands)
                     | _ => nodeCands >/ List.filter (fn x => G.degree graph x < k)
                                      >/ foldl (fn (node, (g, stk, cands)) =>
                                                   (G.removeNode(graph, G.getNodeID node),
                                                    node::stk,
                                                    cands @ G.adj' g node))
                                      (graph, nodeStk, [])
                                      >/ helper
                val (graph', nodeStk', _) = helper(graph, nodeStk, G.nodes graph)
            in
                coalesce(updateIgraph igraph graph', nodeStk')
            end
        and coalesce (igraph, nodeStk) =
            freeze (igraph, nodeStk)
        and freeze (igraph, nodeStk) =
            potentialSpill (igraph, nodeStk)
        and potentialSpill (igraph, nodeStk) =
            let
                val {graph, ...} = extractIgraph igraph
                val nList = G.nodes(graph)
                fun MaxDegree [] = ErrorMsg.impossible "empty IGraph NodeList"
                    | MaxDegree [h] = h
                    | MaxDegree (h::l) =
                        let
                            val x = G.degree(h)
                            val y = G.degree(MaxDegree l)
                        in
                            if x > y then h else (MaxDegree l)
                        end
                val snode = MaxDegree nList
                val newStk = snode::nodeStk
                val newGraph = G.removeNode(graph, G.getNodeID(snode))
            in
                case G.nodes newGraph of
                    hd::tl => simplify(updateIgraph igraph newGraph, newStk)
                  | nil => select(G.empty, newStk)
            end
        and select (cgraph: cGraph, cnode_head::cnode_tail : t_cnode List) = 
            let
                val nid = G.getNodeID(cnode_head) 
                val (c_temp, c_num) = G.nodeInfo(cnode_head)
                fun pick_candi_color (color_list: int list) =
                    let
                        fun helper(hd::tl, candi) =
                            if hd = candi then helper(tl, candi+1) else candi
                        | helper(nil, candi) = candi
                    in
                        helper(color_list, 0)
                    end
                val candi_num = G.adj' cgraph cnode_head 
                    >/ map G.nodeInfo
                    >/ map #2 (* get color_num *)
                    >/ ListMergeSort.uniqueSort Int.compare (* sort colors *)
                    >/ pick_candi_color
                val _ = assert(candi_num < k) (* error and exit if >= k *)
                val new_cgraph = G.changeNodeData(cgraph, nid, (c_temp, candi_num))
            in
                select (new_cgraph, cnode_tail)
            end
            | select (cgraph, nil) = actualSpill cgraph (* some nodes may have color num =-1 or >= k *)
        and actualSpill (cgraph: cGraph)  =
            (instrs, regAlloc(cgraph)) 
        and regAlloc (cgraph: cGraph) =
        let
          val regList = F.callersaveRegsExtra @ F.callersaveRegs @ F.calleesaveRegs
          val cnumList = List.tabulate(18, fn x => x)
          val cnumRegMap = ListPair.foldl (fn (reg, cnum, mp) =>
          IntBinaryMap.insert(mp, cnum, Frame.temp2str reg)) (Temp.temp IntBinaryMap.empty)
          (regList, cnumList)
        in
          G.foldNodes 
          (fn (n, tbl) => Temp.Map.insert(tbl, n >/ G.nodeInfo >/ #1, 
                                               n 
                                               >/ G.nodeInfo 
                                               >/ #2
                                               >/ (fn x =>
                                                 IntBinaryMap.find(cnumRegMap,
                                                 x))
                                         )
          ) Temp.Map.empty cGraph 
        end
        val nodeStk = simplify(graph, []) >/ #2
    in
        (* build -> simplify -> coalesce -> freeze 
           -> potentialSpill -> select -> actualSpill
           -> (color | regAlloc) *)
        build(instrs)
    end
