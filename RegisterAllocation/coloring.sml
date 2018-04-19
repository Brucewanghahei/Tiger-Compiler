structure Coloring =
struct

structure G = Flow.Graph
structure L = Liveness

(* infix op have to be declared in every module *)
infixr 3 </ fun x </ f = f x (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)

fun color (instrs, graph, k) =
    let
        fun simplify (igraph: L.igraph, nodeStk: t_inode List): L.igraph * t_inode list =
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
        and freeze (igraph, nodeStk) =
        and potentialSpill (igraph, nodeStk) =
        and select (cgraph: cGraph): bool * cGraph * Temp.temp list =
        and actualSpill (cgraph: cGraph): bool * cGraph * Temp.temp list =
        val nodeStk = simplify(graph, []) >/ #2
    in
    end
