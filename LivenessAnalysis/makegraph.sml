structure MakeGraph:
          sig
              val instrs2graph: Assem.instr list ->
                                Flow.flowgraph * Flow.Graph.node list
          end
=
struct

structure A = Assem
structure F = Flow
structure T = Temp

fun instrs2graph instrs =
    let
        fun sequentialScan instrs: A.instr list
                                   -> F.Graph * ((T.label list, F.nodeID) list * (T.label * F.nodeID) list) =
            let
                fun scan (instr, (id, graph, jumpsNodes, labelNodes)) =
                    let val (dstOpt, srcOpt, jumpOpt, labelOpt) =
                            case instr of A.OPER{_, dst, src, jump} => (SOME dst, SOME src, jump, NONE)
                                        | A.LABEL{_, lab} => (NONE, NONE, NONE, SOME lab)
                                        | _ => (NONE, NONE, NONE)
                        val graph' = F.Graph.addNode(graph, id)
                        val graph'' = if id = 0 then graph'
                                      else addEdge(graph', {from: id - 1, to: id}),
                            case dstOpt of
                                SOME dst => (
                             )
                              | NONE => (def.tn)
                    in
                    (
                      id + 1,
                      (* to do: add def, use info using changeNodeData*)
                      case jumpOpt of
                          SOME jumps => (jumps, id)::jumpsNodes
                        | NONE => jumpsNodes,
                      case labelOpt of
                          SOME label => (label, id)::labelNodes
                        | NONE => labelNodes,
                    )
                    end
            
(* first pass:
 * create all nodes
 * connect adjacent nodes of instrs
 * return all jump instrs and label instrs with their nodes
 *)
end
