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
        fun coalesce (lgraph: L.igraph): bool * L.igraph * Temp.temp list =
        fun freeze (lgraph: L.igraph): bool * L.igraph * Temp.temp list =
        fun potentialSpill (lgraph: L.igraph): bool * L.igraph * Temp.temp list =
        fun select (cgraph: cGraph): bool * cGraph * Temp.temp list =
        fun actualSpill (cgraph: cGraph): bool * cGraph * Temp.temp list =
        fun liveGraph2ColorGraph (lgraph: L.igraph) = 
        val graph' = simplify graph
    in
        (instrs,
         liveGraph2ColorGraph lGraph') >/ select
                                       >/ #2
    end
