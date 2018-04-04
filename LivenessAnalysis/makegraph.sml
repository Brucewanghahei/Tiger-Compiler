structure MakeGraph:
          sig
              val instrs2graph: Assem.instr list ->
                                Flow.flowgraph * Flow.Graph.node list
          end
=
struct

structure A = Assem
structure F = Flow
structure G = F.Graph
structure T = Temp

(* node info:
 * {
 * def: Temp.temp list,
 * use: Temp.temp list,
 * move: Temp.temp Option (* SOME src *)
 * }
 *)
fun instrs2graph instrs =
    let
        (* first pass:
         * create all nodes
         * connect adjacent nodes of instrs which don't jump
         * return all jump instrs and label instrs with their nodes
         *)
        fun sequentialScan instrs: A.instr list
                                   -> G * (T.label list * F.nodeID) list * (T.label * F.nodeID) list =
            let
                fun scan (instr, (id, graph, jumpsNodes, labelNodes, def, use)) =
                    let val (dstOpt, srcOpt, jumpOpt, labelOpt) =
                            case instr of A.OPER{dst, src, jump, ...} => (SOME dst, SOME src, jump, NONE, false)
                                        | A.LABEL{lab, ...} => (NONE, NONE, NONE, SOME lab, false)
                                        | A.MOVE{dst, src, ...} => (NONE, NONE, SOME [dst], SOME [src], false)
                        val moveOpt = if isMove then
                                        case srcOpt of SOME (src::tl) => SOME src
                                                     | _ => ErrorMsg.impossible "Error when extracting A.MOVE"
                                      else NONE
                        val def' = case dstOpt of
                                      SOME dst => dst @ def
                                    | NONE => def
                        val use' = case srcOpt of
                                      (* use before first def *)
                                      SOME src => (List.filter
                                                      (fn s => case List.find (fn x => x = s) def of
                                                                   SOME _ => false
                                                                 | NONE => true)
                                                      src)@use
                                    | NONE => use
                        val (graph', node) = G.addNode(graph, id,
                                                       {
                                                           def=def,
                                                           use=use,
                                                           move = moveOpt
                                                       })
                        (* connect adjacent instrs if current instr doesn't jump *)
                        val graph'' = if id = 0 orelse isSome jumpOpt then graph'
                                      else G.addEdge(graph', {from = id - 1, to = id}),

                    in
                    (
                      id + 1,
                      case jumpOpt of
                          SOME jumps => (jumps, id)::jumpsNodes
                        | NONE => jumpsNodes,
                      graph'',
                      case labelOpt of
                          SOME label => (label, id)::labelNodes
                        | NONE => labelNodes,
                      def',
                      use'
                    )
                    end
                val (_, graph, jumpsNodes, labelNodes, _, _)
                    = foldl scan (0, G.empty, [], [], [], []) instrs
            in
                (graph, jumpsNodes, labelNodes)
            end
        (* second pass:
         * find label of nodes for each jump label and connect jump nodes with label nodes
         *)
        fun makeJumps (graph, jumpsNodes, labelNodes) =
            let fun addJumpEdges (jumps, from) =
                    foldl (fn (jump, graph) => 
                              labelNodes >/ List.filter (fn (label, _) => label = jump)
                                         >/ map #2
                                         >/ foldl (fn (to, g) => G.addEdge(g, {from = from, to = to})) graph
                          ) graph jumps
            in
                foldl addJumpEdges graph jumpsNodes
            end
    in
        instrs >/ sequentialScan
               >/ makeJumps
    end
end
