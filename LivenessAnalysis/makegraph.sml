structure MakeGraph:
          sig
              val instrs2graph: Assem.instr list -> Flow.flowgraph
          end
=
struct

structure A = Assem
structure F = Flow
structure G = F.Graph
structure T = Temp
structure TSet = Temp.Set


(* type node = {def: Temp.temp list, use: Temp.temp list,  *)
(*              move: (Temp.temp * Temp.temp) option} *)

(* infix op have to be declared in every module *)
infixr 3 </ fun x </ f = f x (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)

fun instrs2graph instrs =
    let
        (* first pass:
         * create all nodes
         * connect adjacent nodes of instrs which don't jump
         * return all jump instrs and label instrs with their nodes
         *)
        fun sequentialScan (instrs: A.instr list):
                                    F.flowgraph * (T.label list * F.nodeID) list * (T.label * F.nodeID) list =
            let
                fun scan (instr, (id, graph, jumpsNodes, labelNodes, def)) =
                    let val (dstOpt, srcOpt, jumpOpt, labelOpt, isMove) =
                            case instr of A.OPER{dst, src, jump, ...} => (SOME dst, SOME src, jump, NONE, false)
                                        | A.LABEL{lab, ...} => (NONE, NONE, NONE, SOME lab, false)
                                        | A.MOVE{dst, src, ...} => (SOME [dst], SOME [src], NONE, NONE, true)
                        val moveOpt = if isMove then
                                        case (dstOpt, srcOpt) of (SOME (dst::_), SOME (src::_)) => SOME (dst, src)
                                                     | _ => ErrorMsg.impossible "Error when extracting A.MOVE"
                                      else NONE
                        val srcSet = case srcOpt of
                                         SOME src => TSet.addList(TSet.empty, src)
                                       | NONE => TSet.empty
                        (* use before first def *)
                        val nodeUse = TSet.difference(srcSet, def)
                        (* don't add to def if used in this node *)
                        val nodeDef = case dstOpt of
                                          SOME dst => TSet.addList(TSet.empty, dst >/ List.filter (fn x => not (TSet.member(srcSet, x))))
                                        | NONE => TSet.empty
                        val (graph', node) = G.addNode'(graph, id,
                                                       {
                                                           def=nodeDef,
                                                           use=nodeUse,
                                                           move = moveOpt
                                                       })
                        (* connect adjacent instrs if current instr doesn't jump *)
                        val graph'' = if id = 0 orelse isSome jumpOpt then graph'
                                      else G.addEdge(graph', {from = id - 1, to = id})

                    in
                    (
                      id + 1,
                      graph'',
                      case jumpOpt of
                          SOME jumps => (jumps, id)::jumpsNodes
                        | NONE => jumpsNodes,
                      case labelOpt of
                          SOME label => (label, id)::labelNodes
                        | NONE => labelNodes,
                      (* record defs in a block *)
                      case labelOpt of
                          SOME _ => TSet.union(def, nodeDef)
                        | NONE => TSet.empty
                    )
                    end
                val (_, graph, jumpsNodes, labelNodes, _)
                    = foldl scan (0, G.empty, [], [], TSet.empty) instrs
            in
                (graph, jumpsNodes, labelNodes)
            end
        (* second pass:
         * find label of nodes for each jump label and connect jump nodes with label nodes
         *)
        fun makeJumps (graph, jumpsNodes, labelNodes) =
            let fun addJumpEdges ((jumps, from), graph) =
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
