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
                val (lgraph', nodeStk', _) = helper(lgraph, nodeStk, G.nodes lgraph)
            in
                coalesce(lgraph', nodeStk')
            end
        and coalesce (lgraph, nodeStk) =
        and freeze (lgraph, nodeStk) =
        and potentialSpill (lgraph, nodeStk) =
        and select (cgraph: cGraph): bool * cGraph * Temp.temp list =
        and actualSpill (cgraph: cGraph): bool * cGraph * Temp.temp list =
        val nodeStk = simplify(graph, []) >/ #2
    in
    end
