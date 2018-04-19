structure Coloring =
struct

structure G = Flow.Graph
structure L = Liveness

(* infix op have to be declared in every module *)
infixr 3 </ fun x </ f = f x (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)

fun color (instrs, graph, k) =
    let
        fun simplify (lgraph: L.igraph): bool * L.igraph * Temp.temp list =
            let fun helper (graph, nodes) =
                    let val toRemove = List.filter (fn x => G.degree graph x < k) nodes
                        val (graph', nodes') = 
                            foldl (fn (node, (g, nextLevel)) =>
                                      (G.removeNode(graph, G.getNodeID node)),
                                   nextLevel @ G.adj' g node)
                                  (graph, []) toRemove
                    in
                        case nodes' of
                             hd::tl => helper(graph', nodes')
                           | nil => coalesce graph'
                    end
            in
                helper lgraph (G.nodes lgraph)
            end
        fun coalesce (lgraph: L.igraph): bool * L.igraph * Temp.temp list =
        fun freeze (lgraph: L.igraph): bool * L.igraph * Temp.temp list =
        fun potentialSpill (lgraph: L.igraph): bool * L.igraph * Temp.temp list =
        fun select (cgraph: cGraph, tp_head::tp_tail : Temp.temp list): bool * cGraph * Temp.temp list =
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


        fun actualSpill (cgraph: cGraph): bool * cGraph * Temp.temp list =
        fun liveGraph2ColorGraph (lgraph: L.igraph) = 
        val graph' = simplify graph >/ #2
        fun t2cnode (t: Temp.temp) :t_cnode =
    in
        (instrs,
         liveGraph2ColorGraph lGraph') >/ select
                                       >/ #2
    end
