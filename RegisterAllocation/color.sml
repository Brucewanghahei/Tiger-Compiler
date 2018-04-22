structure Color =
struct

structure G = Flow.Graph
structure L = Liveness
structure F = MipsFrame
type allocation = string Temp.map
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
  if (not exp) then ErrorMsg.impossible msg
  else ()

fun color (instrs: Assem.instr list,
           initAllocation: allocation,
           registers: Temp.temp list) =
    let
        val k = List.length registers
        fun genIGraph(instrs) = 
                    instrs >/ MakeGraph.instrs2graph
                    >/ Liveness.interferenceGraph (* return (igraph, lgraph) *)
                    >/ #1
        (* remove precolored temp from origin_igraph *)
        val origin_igrpah = 
        let
          val igraph = genIGraph(instrs)
          val {graph, ...} = extractIgraph igraph
          val new_graph = 
          G.foldNodes (fn (node, g) => case Temp.Map.find(initAllocation, node >/
          G.nodeInfo) of
               SOME(_) => G.remove(g, node) | NONE => g) 
               graph graph
        in
          updateIgraph igraph new_graph
        end
        fun igraph2cgraph (igraph: L.igraph) :cGraph =
        let
          val {graph, ...} = extractIgraph igraph
        in
          G.transgraph(graph, fn tp => (tp, ~1))
        end
        fun build(igraph: L.igraph) = 
          simplify(igraph, [])
        and simplify (igraph: L.igraph, nodeStk: t_inode list) =
            let 
              fun n2s n = Int.toString (G.getNodeID n)
                val {graph, ...} = extractIgraph igraph
                fun helper (graph, nodeStk, nodeCands) =
                    case nodeCands of
                        nil => (graph, nodeStk, nodeCands)
                     | _ => nodeCands >/ List.filter (fn x => G.degree x < k
              andalso G.hasNode(graph, x))
                                      >/ foldl (fn (node, (g, stk, cands)) =>
                                      case G.hasNode(g, node) of 
                                          true =>
                                                   (G.removeNode(g, G.getNodeID node),
                                                    node::stk,
                                                    cands @ G.adj' g
                                                    (G.getNode(g, G.getNodeID
                                                    node)))
                                        | false =>
                                                   (g,
                                                   stk,
                                                   cands)
                                      )
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
                        (*
                val snode = MaxDegree nList
                val newStk = snode::nodeStk
                val newGraph = G.removeNode(graph, G.getNodeID(snode))
                *)
            in
              (*
                case g.nodes newgraph of
                    hd::tl => simplify(updateIgraph igraph newgraph, newstk)
                  | nil => select(igraph2cgraph origin_igrpah, newstk)
                  *)
                case nList of
                    hd::tl =>
                      let 
                        val snode = MaxDegree nList
                        val newStk = snode::nodeStk
                        val newGraph = G.removeNode(graph, G.getNodeID(snode))
                      in
                        simplify(updateIgraph igraph newGraph, newStk)
                      end
                  | nil => select(igraph2cgraph origin_igrpah, nodeStk)
            end
        and select (cgraph: cGraph, inode_head::inode_tail :t_inode list) = 
            let
                val nid = G.getNodeID(inode_head) 
                val cnode = G.getNode(cgraph, nid)
                val i_temp = G.nodeInfo(inode_head)
                fun pick_candi_color (color_list: int list) =
                    let
                        fun helper(hd::tl, candi) =
                            if hd = candi then helper(tl, candi+1) else
                              helper(tl, candi)
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
          val cnumList = List.tabulate(k, fn x => x)
          val cnumRegMap = ListPair.foldl (fn (reg, cnum, mp) =>
          IntBinaryMap.insert(mp, cnum, F.temp2str reg)) IntBinaryMap.empty
          (registers, cnumList)
        in
          G.foldNodes 
          (fn (n, tbl) => Temp.Map.insert(tbl, n >/ G.nodeInfo >/ #1, 
                                               n 
                                               >/ G.nodeInfo 
                                               >/ #2
                                               >/ (fn x =>
                                                      case IntBinaryMap.find(cnumRegMap, x) of
                                                          SOME v => v
                                                        | NONE => ErrorMsg.impossible "Registers allocation failed")
                                         )
          ) initAllocation cgraph
        end
    in
        (* build -> simplify -> coalesce -> freeze 
           -> potentialSpill -> select -> actualSpill
           -> (color | regAlloc) *)
        build(origin_igrpah)
    end

fun print_cgraph(cgraph:cGraph) = 
  G.foldNodes (fn (cnode, _) => let val (tp, cn) = G.nodeInfo(cnode) in print(F.temp2str
  tp ^ " " ^ (Int.toString cn)) end) () cgraph

fun print_regAlloc(alloc_map:allocation) =
(
print "=========================\n";
print "Register Allocation\n";
Temp.Map.appi (fn (tp, reg) => print(F.temp2str tp ^ " <- " ^ reg ^"\n"))
alloc_map
)
end
