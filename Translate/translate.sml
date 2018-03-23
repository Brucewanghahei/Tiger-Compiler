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
  
  val nilExp = (Ex Tr.CONST(0))
  
  fun cond_stm (genstm: Tp.label * Tp.label -> Tr.stm) r t f Tr.stm =
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
          Tr.ESEQ(cond_stm(genstm r t f),  Tr.TEMP r)
        end
    | unEX (Nx s) = Tr.ESEQ(s, Tr.CONST 0)

  fun unNx (Ex e) = Tr.EXP e
    | unNx (Nx stm) = stm
    | unNx (Cx genstm) = Tr.EXP(unEx (Cx genstm)) 

  fun unCx (Ex e) = (fn (t: Tp.label, f: Tp.label) => 
    Tr.CJUMP(Tr.EQ, e, Tr.CONST(1), t, f))
    | unCx (Cx genstm) = genstm
    | unCx (Nx _) = (Error.impossible "trying unCx to Nx"; fn (t,f) =>
        Tr.LABEL(t)) (* return a dummy Tree.stm *)


  datatype level = level of level option * Frame.frame * unit ref (* parent_level * unique *)

  datatype access = access of level * Frame.access

  val outermost = level(NONE, Frame.newFrame(Tp.newLabel(), []) ,ref ())

  val fragments: Frame.frag list ref = ref []

  fun newLevel {parent, name, escapes} =
    level(parent, Frame.newFrame(name, escapes), ref ())

  fun formals (level(level_opt, frame, uniq)) =
    Frame.formals(frame)

  fun intlit(i) = Ex(Tr.CONST(i))

  fun allocLocal (level(level_opt, frame, uniq)) escape =
    access(level(level_opt, frame, uniq), Frame.allocLocal(frame, escape))  

  fun trace_levels (inside_level, outside_level, fp) access =
  let
    fun trace (curr_level(SOME(parent_level), _, curr_uniq_ref), target_level(_, _,
      target_uniq_ref)) exp =
      if curr_uniq_ref = target_uniq_ref then
        Ex(Frame.exp access exp)
      else
        (* static link offset is 0 *)
        trace (parent_level, target_level) (Tr.MEM exp)
  in
    trace (inside_level, outside_level, Tr.TEMP(fp))
  end

  fun seq tree_exp::tree_exp_tail =
    Tr.SEQ(tree_exp, seq tree_exp_tail)
    | seq tree_exp = tree_exp
  
  fun simpleVar (access(def_lev, fr_acc), call_lev) =
    trace_levels (call_lev, def_lev, Frame.FP) fr_acc

  fun subVar (base_fp: exp, offset: exp) =
    Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, unEx(base_fp), unEx(offset))))

  fun assign (vExp, eExp) = Nx(Tr.MOVE(vExp, eExp))

  fun move base offset rval = Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.TEMP(base),
    Tr.CONST(offset))), rval)

  fun createArray (init_exp, size_exp) = 
      Frame.externalCall("initArray", [size_exp, init_exp])
    
  fun strlit s =
      let
          val sfrag = List.find (fn e =>
                                      case e of
                                          Frame.PROC => false
                                        | Frame.STRING(_, s') => s = s')
                                  !fragments
      in
          case sfrag of
              SOME (Frame.STRING(label, _)) => Ex(Tr.NAME(label))
            | NONE => let val newLabel = Tp.newLabel()
                      in
                          fragments := Frame.STRING(newLabel, s)::!fragments;
                          Ex(Tr.NAME(newLabel))
                      end
      end

  fun createRecord field_list =
  let
    val base = Tp.newtemp()
    val head = Tr.MOVE(Tr.TEMP(base), Frame.externalCall("malloc",
    [Tr.CONST(List.length field_list)]))
    val nodes = List.rev(foldl (fn (field, (nodes, k)) => 
      ((move base k field)::nodes, k+1)) ([], 0) filed_list)
  in
    Ex(Tr.ESEQ(seq (head::nodes), Tr.TEMP(base)))
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
      | ari _ = (Error.impossible "arithOp not support";
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
      | comp _ = (Error.impossible "compOp not support";
                dummy_exp)
  in
    comp oper
  end

  fun whileExp (test, body) =
      let
      in
      end

  fun breakExp bL = Nx(Tr.JUMP(Tr.NAME(bL), [bL]))

  fun ifExp (cond, thenExp) =
      let
        val cond = unCx(cond)
        val r = Tp.newtemp()
        val thenLabel = Tp.newLabel()
        val endLabel = Tp.newLabel()
      in
        Ex (Tr.ESEQ(seq[(cond) (thenLabel, endLabel),
                    Tr.LABEL(thenLabel),
                    Tr.MOVE (Tr.TEMP(r), thenExp),
                    Tr.LABEL(endLabel)],
            Tr.TEMP(r)))
      end

  fun ifelseExp (cond, thenExp, elseExp) =
      let
        val cond = unCx(cond)
        val r = Tp.newtemp()
        val thenLabel = Tp.newLabel()
        val elseLabel = Tp.newLabel()
        val joinLabel = Tp.newLabel()
      in
        Ex (Tr.ESEQ(seq[(cond) (thenLabel, elseLabel),
                    Tr.LABEL(thenLabel),
                    Tr.MOVE(Tr.TEMP(r), unEx(thenExp)),
                    Tr.JUMP(Tr.NAME(endLabel), [endLabel]),
                    Tr.LABEL(elseLabel),
                    Tr.MOVE(Tr.TEMP(r), unEx(elseExp)),
                    Tr.LABEL(endLabel)],
            Tr.TEMP(r)))
      end

  fun forExp (var, escape, lo, hi, body) =
      let
        val bodyLabel = Tp.newLabel()
        val forLabel = Tp.newLabel()
      in
        Nx (seq[Tr.MOVE(unEx(var), unEx(lo)),
                Tr.CJUMP(Tr.LE, unEx(var), unEx(hi), bodyLabel, escape)
                Tr.LABEL(bodyLabel),
                unEx(body),
                Tr.CJUMP(Tr.LT, unEx(var), unEx(hi), forLabel, escape),
                Tr.LABEL(forLabel),
                Tr.MOVE(unEx(var), Tr.BINOP(Tr.PLUS, unEx(var), Tr.CONST 1)),
                Tr.JUMP(Tr.NAME(forLabel), [forLabel]),
                Tr.LABEL(escape)])
      end

end
