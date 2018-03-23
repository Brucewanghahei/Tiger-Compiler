structure Translate : TRANSLATE =
struct
  structure Frame = MipsFrame
  structure Tr = Tree
  structure Tp = Temp
  structure A = Absyn

  datatype exp = Ex of Tr.exp
               | Nx of Tr.stm
               | Cx of Tp.label * Tp.label -> Tr.stm

  val dummy_exp = Nx(Tr.EXP(Tr.CONST(0)))
  
  val nilkw = (Ex (Tr.CONST 0))

  fun seq (tree_stm::tree_stm_tail) =
    Tr.SEQ(tree_stm, seq tree_stm_tail)
    | seq [tree_stm] = tree_stm
  
  fun cond_stm (genstm: Tp.label * Tp.label -> Tr.stm) r t f =
    seq[Tr.MOVE(Tr.TEMP r, Tr.CONST 1),
        genstm(t,f),
        Tr.LABEL f,
        Tr.MOVE(Tr.TEMP r, Tr.CONST 0),
        Tr.LABEL t]

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
        let val r = Tp.newtemp()
            val t = Tp.newlabel() and f = Tp.newlabel()
        in
          Tr.ESEQ(cond_stm genstm r t f,  Tr.TEMP r)
        end
    | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0)

  fun unNx (Ex e) = Tr.EXP e
    | unNx (Nx stm) = stm
    | unNx (Cx genstm) = Tr.EXP(unEx (Cx genstm)) 

  fun unCx (Ex e) = (fn (t: Tp.label, f: Tp.label) => 
    Tr.CJUMP(Tr.EQ, e, Tr.CONST(1), t, f))
    | unCx (Cx genstm) = genstm
    | unCx (Nx _) = (ErrorMsg.impossible "trying unCx to Nx"; fn (t,f) =>
        Tr.LABEL(t)) (* return a dummy Tree.stm *)


  datatype level = level of level option * Frame.frame * unit ref (* parent_level * unique *)

  datatype access = access of level * Frame.access

  val outermost = level(NONE, Frame.newFrame {name=Tp.newlabel(), escapes=[]} ,ref ())

  val fragments: Frame.frag list ref = ref []
  
  fun getResult () =
    ! fragments

  fun newLevel {parent, name, escapes} =
    level(SOME(parent), Frame.newFrame {name=name, escapes=escapes}, ref ())

  fun formals (level(level_opt, frame, uniq)) =
    map (fn formal => access(level(level_opt, frame, uniq), formal)) (Frame.formals frame)

  fun intlit(i) = Ex(Tr.CONST(i))

  fun allocLocal (level(level_opt, frame, uniq)) escape =
    access(level(level_opt, frame, uniq), Frame.allocLocal frame escape)  

  fun procEntryExit (body, level(l, frame, uniq)) =
      let val body' = Frame.procEntryExit1(frame, Tr.MOVE(Tr.TEMP Frame.RV, unEx(body)))
      in
          fragments := Frame.PROC{body = body', frame = frame} :: !fragments
      end

  fun get_static_link (call_level, def_level, fp) =
  let
    fun trace (cur_level, tgt_level) exp=
    let
      val level(SOME(prt_level), _, cur_unqref) = cur_level
      val level(_, _, tgt_unqref) = tgt_level
    in
      if cur_unqref = tgt_unqref then
        exp
      else
        (* static link offset is 0 *)
        trace (prt_level, tgt_level) (Tr.MEM exp)
    end
  in
    trace (call_level, def_level) (Tr.TEMP fp)
  end

  fun trace_levels (call_level, def_level, fp) frame_access =
    Ex(Frame.exp frame_access
      (get_static_link(call_level, def_level, fp))
    )
  
  fun simpleVar (access(def_lev, fr_acc), call_lev) =
    trace_levels (call_lev, def_lev, Frame.FP) fr_acc

  fun subVar (base_fp: exp, offset: exp) =
    Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, unEx(base_fp), unEx(offset))))

  fun assign (vExp, eExp) = Nx(Tr.MOVE(unEx vExp, unEx eExp))

  fun createArray (init_exp, size_exp) = 
      Ex (Frame.externalCall("initArray", [unEx size_exp, unEx init_exp]))
    
  fun strlit s =
      let
          val sfrag = List.find (fn e =>
                                      case e of
                                          Frame.PROC _ => false
                                        | Frame.STRING(_, s') => s = s')
                                  (!fragments)
      in
          case sfrag of
              SOME (Frame.STRING(label, _)) => Ex(Tr.NAME(label))
            | NONE => let val newlabel = Tp.newlabel()
                      in
                          fragments := Frame.STRING(newlabel, s)::(!fragments);
                          Ex(Tr.NAME(newlabel))
                      end
      end

  fun move base offset rval = Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.TEMP(base),
    Tr.CONST(offset))), rval)

  fun createRecord field_list =
  let
    val base = Tp.newtemp()
    val head = Tr.MOVE(Tr.TEMP(base), Frame.externalCall("malloc",
    [Tr.CONST((List.length field_list)*Frame.wordSize)]))
    val (nodes, k) = foldl (fn (field, (nodes, k)) => 
      ((move base (k*Frame.wordSize) (unEx field))::nodes, k+1)) ([], 0) field_list
  in
    Ex(Tr.ESEQ(seq (head::(List.rev nodes)), Tr.TEMP(base)))
  end

  fun letexp (dec_exps, body_exp) = 
    Ex(Tr.ESEQ(seq (map unNx dec_exps), unEx body_exp))

  fun arithOp (oper, lexp, rexp) =
  let
    fun gen oper = Ex(Tr.BINOP(oper, unEx lexp, unEx rexp))
    fun ari A.PlusOp = gen Tr.PLUS
      | ari A.MinusOp = gen Tr.MINUS
      | ari A.TimesOp = gen Tr.MUL
      | ari A.DivideOp = gen Tr.DIV
      | ari _ = (ErrorMsg.impossible "arithOp not support";
                dummy_exp)
  in
    ari oper
  end

  fun compOp (oper, lexp, rexp) =
  let
    fun gen oper = Cx(
      fn (t,f) => Tr.CJUMP(oper, unEx lexp, unEx rexp, t, f))
    fun comp A.EqOp = gen Tr.EQ
      | comp A.NeqOp = gen Tr.NE
      | comp A.LtOp = gen Tr.LT
      | comp A.LeOp = gen Tr.LE
      | comp A.GtOp = gen Tr.GT
      | comp A.GeOp = gen Tr.GE
      | comp _ = (ErrorMsg.impossible "compOp not support";
                dummy_exp)
  in
    comp oper
  end

  fun whileExp (test, body) =
      let
        val testLabel = Tp.newlabel()
        val bodyLabel = Tp.newlabel()
        val doneLabel = Tp.newlabel()
      in
        Nx (seq[Tr.LABEL(testLabel),
                unCx(test) (bodyLabel, doneLabel),
                Tr.LABEL(bodyLabel),
                unNx(body),
                Tr.JUMP(Tr.NAME(testLabel), [testLabel]),
                Tr.LABEL(doneLabel)])
      end

  fun breakExp bL = Nx(Tr.JUMP(Tr.NAME(bL), [bL]))

  fun ifExp (cond, thenExp) =
      let
        val cond = unCx(cond)
        val r = Tp.newtemp()
        val thenLabel = Tp.newlabel()
        val endLabel = Tp.newlabel()
      in
        Ex (Tr.ESEQ(seq[(cond) (thenLabel, endLabel),
                    Tr.LABEL(thenLabel),
                    Tr.MOVE (Tr.TEMP(r), unEx(thenExp)),
                    Tr.LABEL(endLabel)],
            Tr.TEMP(r)))
      end

  fun ifelseExp (cond, thenExp, elseExp) =
      let
        val cond = unCx(cond)
        val r = Tp.newtemp()
        val thenLabel = Tp.newlabel()
        val elseLabel = Tp.newlabel()
        val joinLabel = Tp.newlabel()
      in
        Ex (Tr.ESEQ(seq[(cond) (thenLabel, elseLabel),
                    Tr.LABEL(thenLabel),
                    Tr.MOVE(Tr.TEMP(r), unEx(thenExp)),
                    Tr.JUMP(Tr.NAME(joinLabel), [joinLabel]),
                    Tr.LABEL(elseLabel),
                    Tr.MOVE(Tr.TEMP(r), unEx(elseExp)),
                    Tr.LABEL(joinLabel)],
            Tr.TEMP(r)))
      end

  fun forExp (var_access, lo, hi, body) =
      let
        val bodyLabel = Tp.newlabel()
        val forLabel = Tp.newlabel()
        val joinLabel = Tp.newlabel()
        val var_ex = Frame.exp var_access (Tree.TEMP Frame.FP)
        val hi_ex = unEx hi
        val lo_ex = unEx lo
      in
        Nx (seq[Tr.MOVE(var_ex, lo_ex),
                Tr.CJUMP(Tr.LE, var_ex, hi_ex, bodyLabel, joinLabel),
                Tr.LABEL(bodyLabel),
                unNx(body),
                Tr.CJUMP(Tr.LT, var_ex, hi_ex, forLabel, joinLabel),
                Tr.LABEL(forLabel),
                Tr.MOVE(var_ex, Tr.BINOP(Tr.PLUS, var_ex, Tr.CONST 1)),
                Tr.JUMP(Tr.NAME(bodyLabel), [bodyLabel]),
                Tr.LABEL(joinLabel)])
      end

  fun call (callLevel, decLevel, label, args) =
      let
          val sl = get_static_link (callLevel, decLevel, Frame.FP)
      in
          Ex (Tr.CALL(Tr.NAME label, sl::(map unEx args)))
      end


end
