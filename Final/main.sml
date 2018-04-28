structure Main = struct

  structure Tr = Translate
  structure F = MipsFrame
  (* structure R = RegAlloc *)

  (* infix op have to be declared in every module *)
  infixr 3 </ fun x </ f = f x (* Right application *)
  infix 1 >/ val op>/ = op</ (* Left pipe *)

  fun load_library ()=
  let
    val libraries = ["sysspim.s", "runtimele.s"]
  in
    libraries
      >/ map (fn fname => TextIO.inputAll (TextIO.openIn fname))
      >/ String.concat
  end

  fun getsome (SOME x) = x
   | getsome NONE = ErrorMsg.impossible "Fail to get value from option"

  fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ (Symbol.name (F.name frame)) ^ "\n")
         (*         val _ = Printtree.printtree(out,body); *)
         val stms = Canon.linearize body
                 (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
         val instrs =   List.concat(map (Mips.codegen frame) stms') 
         val instrs2 = F.procEntryExit2(frame, instrs)
         val (instrs3, alloc) = Color.color(instrs2, F.tempMap, F.user_registers) 
         val flowgraph = MakeGraph.instrs2graph(instrs2)
         val (infegraph, livegraph) = Liveness.interferenceGraph(flowgraph)
         val {prolog, body = bodyInstrs, epilog} = F.procEntryExit3(frame, instrs2)
         val format0 = Assem.format(Temp.makestring)
         val format1 = Assem.format(F.temp2str)
     in
         TextIO.output(out,prolog);
         app (fn i => Printtree.printtree(TextIO.stdOut, i)) stms';
         app (fn i => TextIO.output(out,format1 i)) instrs3;
         app (fn i => TextIO.output(TextIO.stdOut,format1 i)) instrs2;
         app (fn i => TextIO.output(TextIO.stdOut,format1 i)) instrs3;
         Color.print_regAlloc(alloc);
         Flow.show flowgraph;
         Liveness.showlive livegraph;
         (*
         Liveness.show infegraph;
         *)
         TextIO.output(out,epilog)
     end
   | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

  fun withOpenFile fname f = 
     let val out = TextIO.openOut fname
     in (f out before TextIO.closeOut out) 
        handle e => (TextIO.closeOut out; raise e)
     end 

  fun compile filename = 
     let val absyn = Parse.parse filename
         val frags = (FindEscape.prog absyn; Semant.transProg absyn)
     in 
         withOpenFile (filename ^ ".s") 
                      (fn out => (
                        app (emitproc out) frags;
                        TextIO.output(out, load_library())
                      ))
     end

end
