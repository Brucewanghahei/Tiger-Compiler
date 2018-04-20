structure Coloring =
struct

structure G = Flow.Graph
structure L = Liveness
structure F = MipsFrame

(* infix op have to be declared in every module *)
infixr 3 </ fun x </ f = f x (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)

fun color (instrs, k) =
    let
        fun build (instrs) = 
        let
          val igraph = 
          instrs >/ MakeGraph.instrs2graph
                 >/ Liveness.interferenceGraph (* return (igraph, lgraph) *)
                 >/ #1
        in
          simplify (igraph, [])
        end
        and simplify (igraph: L.igraph, nodeStk: t_inode List): L.igraph * t_inode list =
            let fun helper (igraph, nodeStk, nodeCands) =
                    case nodeStk of
                        nil => (igraph, nodeStk, nodeCands)
                     | _ => nodeCands >/ List.filter (fn x => G.degree igraph x < k)
                                      >/ foldl (fn (node, (g, stk, cands)) =>
                                                   (G.removeNode(graph, G.getNodeID node),
                                                    node::stk,
                                                    cands @ G.adj' g node))
                                      (igraph, nodeStk, [])
                                      >/ helper
                val (igraph', nodeStk', _) = helper(igraph, nodeStk, G.nodes igraph)
            in
                coalesce(igraph', nodeStk')
            end
        and coalesce (igraph, nodeStk) =
          freeze (igraph, nodeStk)
        and freeze (igraph, nodeStk) =
          potentialSpill (igraph, nodeStk)
        and potentialSpill (igraph, nodeStk) =
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
        | select (cgraph, nil) = actualSpill cgraph (* some nodes may have color
        num =-1 or >= k *)
        and t2cnode (t: Temp.temp) :t_cnode =
        and actualSpill (cgraph: cGraph)  =
          (instrs, regAlloc(cgraph)) 
        and regAlloc (cgraph: cGraph) =
        val nodeStk = simplify(graph, []) >/ #2
    in
        (* build -> simplify -> coalesce -> freeze 
           -> potentialSpill -> select -> actualSpill
           -> (color | regAlloc) *)
        build(instrs)
    end
