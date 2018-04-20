structure Coloring =
struct

structure G = Flow.Graph
structure L = Liveness
structure F = MipsFrame
type allocation = F.register Temp.map
type t_cnode = Temp.temp * int (* tmp, color *)
type t_inode = L.t_inode 
type cGraph = t_cnode G.graph

(* infix op have to be declared in every module *)
infixr 3 </ fun x </ f = f x (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)

fun extractIgraph (L.IGRAPH{graph, tnode, gtemp, moves}) =
    {graph = graph, tnode = tnode, gtemp = gtemp, moves = moves}
    (*
  | extractIgraph _ = ErrorMsg.impossible "extract IGRAPH"
  *)

fun updateIgraph (L.IGRAPH{graph, tnode, gtemp, moves}) graph' =
    L.IGRAPH{graph = graph',
             (* warning: tnode is not updated! Should not use if coalesce is implemented *)
             tnode = tnode,
             gtemp = gtemp,
             moves = moves >/ List.filter (fn (x, y) =>
                                              G.hasNode(graph', x)
                                              andalso G.hasNode(graph', y))
            }

fun assert (exp, msg) = 
  if (exp) then ErrorMsg.impossible msg
  else ()

fun color (instrs, k) =
    let
        fun genIGraph(instrs) = 
                    instrs >/ MakeGraph.instrs2graph
                    >/ Liveness.interferenceGraph (* return (igraph, lgraph) *)
                    >/ #1
        val origin_igrpah = genIGraph(instrs)
        fun igraph2cgraph (igraph: L.igraph) :cGraph =
        let
          val {graph, ...} = extractIgraph igraph
        in
          G.transgraph(graph, fn tp => (tp, ~1))
        end
        fun build(igraph: L.igraph) = 
          simplify(igraph, [])
        and simplify (igraph: L.igraph, nodeStk: t_inode list) =
            let val {graph, ...} = extractIgraph igraph
                fun helper (graph, nodeStk, nodeCands) =
                    case nodeStk of
                        nil => (graph, nodeStk, nodeCands)
                     | _ => nodeCands >/ List.filter (fn x => G.degree x < k)
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
                  | nil => select(igraph2cgraph origin_igrpah, newStk)
            end
        and select (cgraph: cGraph, inode_head::inode_tail :t_inode list) = 
            let
                val nid = G.getNodeID(inode_head) 
                val cnode = G.getNode(cgraph, nid)
                val i_temp = G.nodeInfo(inode_head)
                fun pick_candi_color (color_list: int list) =
                    let
                        fun helper(hd::tl, candi) =
                            if hd = candi then helper(tl, candi+1) else candi
                        | helper(nil, candi) = candi
                    in
                        helper(color_list, 0)
                    end
                val candi_num = G.adj' cgraph cnode 
                    >/ map G.nodeInfo
                    >/ map #2 (* get color_num *)
                    >/ ListMergeSort.uniqueSort Int.compare (* sort colors *)
                    >/ pick_candi_color
                val _ = assert(candi_num < k, "actual spill") (* error and exit if >= k *)
                val new_cgraph = G.changeNodeData(cgraph, nid, (i_temp, candi_num))
            in
                select (new_cgraph, inode_tail)
            end
            | select (cgraph, nil) = actualSpill cgraph (* some nodes may have color num =-1 or >= k *)
        and actualSpill (cgraph: cGraph)  =
            (instrs, regAlloc(cgraph)) 
        and regAlloc (cgraph: cGraph) =
        let
          val regList = F.callersaveRegsExtra @ F.callersaveRegs @ F.calleesaveRegs
          val cnumList = List.tabulate(18, fn x => x)
          val cnumRegMap = ListPair.foldl (fn (reg, cnum, mp) =>
          IntBinaryMap.insert(mp, cnum, F.temp2str reg)) IntBinaryMap.empty
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
          ) Temp.Map.empty cgraph 
        end
        (*
        val nodeStk = simplify(graph, []) >/ #2
        *)
    in
        (* build -> simplify -> coalesce -> freeze 
           -> potentialSpill -> select -> actualSpill
           -> (color | regAlloc) *)
        build(origin_igrpah)
    end
end
