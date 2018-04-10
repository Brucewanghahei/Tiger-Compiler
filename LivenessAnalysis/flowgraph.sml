structure Flow =
struct

type nodeID = int

structure NodeOrd =
struct
type ord_key = nodeID
val compare = Int.compare
end

structure Graph = FuncGraph(NodeOrd)
structure TSet = Temp.Set

(* move: (dst * src) Option
 * case SOME _ => dst->src is a move edge
 * | NONE => no move edge  *)
type t_node = {def: TSet.set, use: TSet.set,
             move: (Temp.temp * Temp.temp) option}
type flowgraph = t_node Graph.graph

fun println x = print (x ^ "\n");
fun printMove (SOME((t1, t2))) = (Temp.makestring t1) ^ "-" ^ (Temp.makestring t2)
  | printMove NONE = ""

val ts2s = Temp.ts2s
fun show flowgraph =(
  println("===================");
  println("Flow Graph");
  Graph.printGraph' (fn (nid, {def=def, use=use, move=move}:t_node)
  => (Int.toString nid) ^ "\ndef: {" ^ (ts2s def)  ^ " }" ^ "\nuse: {" ^
  (ts2s use) ^ " }" ^ "\nmove: {" ^ (printMove move) ^ " }") flowgraph)

(* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

    Data of nodes are stored in the graph

 *)

end
