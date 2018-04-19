structure Coloring =
struct

structure G = Flow.Graph
structure L = Liveness

(* infix op have to be declared in every module *)
infixr 3 </ fun x </ f = f x (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)

fun color (instrs, graph, k) =
    let
        fun simplify (lgraph: L.igraph, nodeStk: t_inode List): L.igraph * t_inode list =
            let fun helper (lgraph, nodeStk, nodeCands) =
                    case nodeStk of
                        nil => (lgraph, nodeStk, nodeCands)
                     | _ => nodeCands >/ List.filter (fn x => G.degree lgraph x < k)
                                      >/ foldl (fn (node, (g, stk, cands)) =>
                                                   (G.removeNode(graph, G.getNodeID node),
                                                    node::stk,
                                                    cands @ G.adj' g node))
                                      (lgraph, nodeStk, [])
                                      >/ helper
                val (lgraph', nodeStk', _) = helper(lgraph, nodeStk, G.nodes lgraph)
            in
                coalesce(lgraph', nodeStk')
            end
        fun select (cgraph: cGraph, tp_head::tp_tail : Temp.temp list) = 
        let
          val c_node = t2cnode(tp_head)
          val nid = G.getNodeID(c_node) 
          fun pick_candi_color (color_list: int list) =
          let
            fun helper(hd::tl, candi) =
              if hd = candi then helper(tl, candi+1) else candi
              | helper(nil, candi) = candi
          in
            helper(color_list, 0)
          end
          val candi_num = G.adj' cgraph c_node 
                          >/ map G.nodeInfo
                          >/ map #2 (* get color_num *)
                          >/ ListMergeSort.uniqueSort Int.compare (* sort colors *)
                          >/ pick_candi_color
          val is_spill:bool = assert(candi_num < k) (* error and exit if >= k *)
        in
          (not is_spill, G.changeNodeData(cgraph, nid, (tp_head, candi_num)), tp_tail)
        end
        | select (cgraph, nil) = (false, cgraph, nil)


        fun t2cnode (t: Temp.temp) :t_cnode =
        and coalesce (lgraph, nodeStk) =
        and freeze (lgraph, nodeStk) =
        and potentialSpill (lgraph, nodeStk) =
        and select (cgraph: cGraph): bool * cGraph * Temp.temp list =
        and actualSpill (cgraph: cGraph): bool * cGraph * Temp.temp list =
        val nodeStk = simplify(graph, []) >/ #2
    in
    end
