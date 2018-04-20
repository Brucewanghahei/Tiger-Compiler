functor FuncGraph(K:ORD_KEY) :> FUNCGRAPH where type nodeID = K.ord_key=
struct
structure Key = K
type nodeID = Key.ord_key
structure NodeSet = SplaySetFn(Key)
structure NodeMap = SplayMapFn(Key)

structure EdgeKey = struct type ord_key = {from:nodeID,to:nodeID}
		    fun compare({from=f1,to=t1},{from=f2,to=t2}) = 
			case Key.compare(f1,f2) of
			    EQUAL => Key.compare(t1,t2)
			  | x => x
		    end
structure EdgeSet = SplaySetFn(EdgeKey)

type 'a node = (nodeID * 'a * NodeSet.set * NodeSet.set)
type 'a graph = 'a node NodeMap.map
type 'a edge = {from: nodeID, to: nodeID}


exception NoSuchNode of nodeID
exception NoSuchEdge of nodeID * nodeID


val empty = NodeMap.empty

fun hasNode(g, (nid, _, _, _)) =
    case NodeMap.find(g, nid) of
        SOME _ => true
      | NONE => false

fun getNode(g,nid) = case NodeMap.find(g,nid) of
			 NONE => raise NoSuchNode(nid)
		       | SOME x=> x

fun setNode(g,nid,d,succs,preds) =
let
  fun l2s l = NodeSet.addList(NodeSet.empty, l)
in
  NodeMap.insert(g,nid,(nid,d,l2s succs,l2s preds))
end

fun addNode(g,nid,d) = NodeMap.insert(g,nid,(nid,d,NodeSet.empty,NodeSet.empty))
fun addNode'(g,nid,d) = 
    let val n = (nid,d,NodeSet.empty,NodeSet.empty)
	val g' = NodeMap.insert(g,nid,n)
    in
	(g',n)
    end
fun changeNodeData(g,nid,d) =
    let val (_,_,s,p) = getNode(g,nid)
    in
	NodeMap.insert(g,nid,(nid,d,s,p))
    end

fun adjustSets(g,{from,to},f) = 
    case Key.compare(from,to) of
	EQUAL => let val (i,d,s,p) = getNode(g,from)
		 in
		     NodeMap.insert(g,from,(i,d,f(s,from),f(p,from)))
		 end
      | _ => let val (fi,fd,fs,fp) = getNode(g,from)
		 val (ti,td,ts,tp) = getNode(g,to)
		 val fs' = f(fs,to) 
		 val tp' = f(tp,from) 
	     in
		 NodeMap.insert(NodeMap.insert(g,from,(fi,fd,fs',fp)),
				to,
				(ti,td,ts,tp'))
	     end
fun addEdge(g,{from,to}) = adjustSets(g,{from=from,to=to},NodeSet.add)
fun removeEdge(g,{from,to}) = adjustSets(g,{from=from,to=to},NodeSet.delete)
			      handle NotFound => raise NoSuchEdge(from,to)
fun removeEdge'(g,{from,to}) = adjustSets(g,{from=from,to=to},NodeSet.delete)
			       handle NotFound => g


fun doubleEdge (g, temp1, temp2) = case Key.compare(temp1,temp2) of
  EQUAL => g
|  _ => let
           val g' = addEdge(g, {from = temp1, to = temp2})
        in
          addEdge(g', {from = temp2, to = temp1})
        end


fun removeNode(g,nid) = 
    let val (_,_,succ,pred) = getNode(g,nid)
	val es1 = NodeSet.foldl (fn(s,es)=>EdgeSet.add(es,{from=nid,to=s})) 
				EdgeSet.empty 
				succ
	val es2 = NodeSet.foldl (fn(p,es)=>EdgeSet.add(es,{from=p,to=nid})) 
				es1
				pred
	val g' = EdgeSet.foldl (fn(e,g)=>removeEdge(g,e)) g es2
	val (g',_) = NodeMap.remove(g',nid) handle NotFound => raise NoSuchNode nid
    in
	g'
    end
fun removeNode'(g,nid) = removeNode(g,nid) handle NoSuchNode nid => g

fun nodeInfo(_,x,_,_) = x

fun outDegree(_,_,s,_) = NodeSet.numItems s
fun inDegree(_,_,_,p) = NodeSet.numItems p
fun degree(_,_,s,p) = (NodeSet.numItems s) + (NodeSet.numItems p)

val nodes = NodeMap.listItems
fun succs (_,_,s,_) = NodeSet.listItems s
fun succs' g n = map (fn(nid)=>getNode(g,nid)) (succs n) 
fun preds (_,_,_,p) = NodeSet.listItems p
fun preds' g n = map (fn(nid)=>getNode(g,nid)) (preds n) 
fun adj (_,_,s,p) = NodeSet.listItems(NodeSet.union(s,p))
fun adj' g n = map (fn(nid)=>getNode(g,nid)) (adj n)

fun getNodeID (n,_,_,_) = n

fun remove(g,n) = removeNode(g, getNodeID n)

val foldNodes = NodeMap.foldl
fun foldSuccs f init (_,_,s,_) = NodeSet.foldl f init s
fun foldSuccs' g f init (_,_,s,_) = NodeSet.foldl (fn(nid,x)=>f(getNode(g,nid),x)) init s
fun foldPreds f init (_,_,_,p) = NodeSet.foldl f init p
fun foldPreds' g f init (_,_,_,p) = NodeSet.foldl (fn(nid,x)=>f(getNode(g,nid),x)) init p
fun foldAdjs f init (_,_,s,p) = NodeSet.foldl f init (NodeSet.union(s,p))
fun foldAdjs' g f init (_,_,s,p) = NodeSet.foldl (fn(nid,x)=>f(getNode(g,nid),x)) init
  (NodeSet.union(s,p))


fun isAdjacent ((n1,_,s1,p1),(n2,_,s2,p2)) = 
  NodeSet.member(NodeSet.union(s1,p1),n2) orelse
  NodeSet.member(NodeSet.union(s2,p2),n1)

fun transgraph (a_graph: 'a graph, trans_f: 'a -> 'b) : 'b graph =
let
  fun f (inode, cgraph) =
  let
    val nid = G.getNodeID(inode)
    val a_info = G.getNodeInfo(inode)
    val b_info = trans_f(a_info)
    val succs = G.succs(inode)
    val preds = G.preds(inode)
  in
    G.setNode(cgraph, nid, b_info, succs, preds)
  end
in
 igraph.foldNodes f G.empty igraph
end

fun printGraph stringify g = 
    let fun println x = print(x ^"\n")
	fun stringNid nid = 
	    let val (_,data,_,_) = getNode(g,nid)
	    in
		"   "^ stringify(nid,data)
	    end
	fun prSet s = NodeSet.app (println o stringNid) s
	fun prOneNode(nid,data,succs,preds) = 
	    let val s = stringify(nid,data)
		val () = println("Node: " ^ s)
		val () = println(" -> Successors:")
		val () = prSet succs
		val () = println(" -> Predecessors:")
		val () = prSet preds
	    in
		()
	    end
    in
	NodeMap.app prOneNode g
    end

fun printUGraph stringify g = 
    let fun println x = print(x ^"\n")
	fun stringNid nid = 
	    let val (_,data,_,_) = getNode(g,nid)
	    in
		"   "^ stringify(nid,data)
	    end
	fun prSet s = NodeSet.app (println o stringNid) s
	fun prOneNode(nid,data,succs,preds) = 
	    let val s = stringify(nid,data)
		val () = println("Node: " ^ s)
		val () = println(" -> Neighbors:")
		val () = prSet succs
	    in
		()
	    end
    in
	NodeMap.app prOneNode g
    end

fun printGraph' stringify g = 
    let fun println x = print(x ^"\n")
	    fun stringNid nid = 
	        let val (_,data,_,_) = getNode(g,nid)
	        in
		        "   "^ stringify(nid,data)
	        end
	    fun prSet s = NodeSet.app (println o stringNid) s
	    fun prOneNode(nid,data,succs,preds) = 
	        let val s = stringify(nid,data)
		        val () = println("Node: " ^ s)
		        (* val () = println(" -> Successors:") *)
		        (* val () = prSet succs *)
		        (* val () = println(" -> Predecessors:") *)
		        (* val () = prSet preds *)
	        in
		        ()
	        end
    in
	    NodeMap.app prOneNode g
    end
  
fun printGraph'' stringify id2s g = 
    let fun println x = print(x ^"\n")
	    fun prSet s = NodeSet.app (fn nid => print((id2s nid) ^ " ")) s
	    fun prOneNode(nid,data,succs,preds) = 
	    let val s = stringify(nid,data)
        in
		  println("Node: " ^ s);
		  print(" -> Succs: ");
		  prSet succs;
		  print("\n -> Preds: ");
		  prSet preds;
          print("\n");
          ()
	    end
    in
	    NodeMap.app prOneNode g
    end
end
