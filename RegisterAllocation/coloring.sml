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
        fun select (cgraph: cGraph): bool * cGraph * Temp.temp list =
        fun actualSpill (cgraph: cGraph): bool * cGraph * Temp.temp list =
        fun liveGraph2ColorGraph (lgraph: L.igraph) = 
        val graph' = simplify graph >/ #2
    in
        (instrs,
         liveGraph2ColorGraph lGraph') >/ select
                                       >/ #2
    end
