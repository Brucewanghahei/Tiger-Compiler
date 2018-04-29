structure Translate : TRANSLATE =
struct
  structure Frame = MipsFrame
  structure Tr = Tree
  structure Tp = Temp
  structure A = Absyn

  type frag = Frame.frag

  datatype exp = Ex of Tr.exp
               | Nx of Tr.stm
               | Cx of Tp.label * Tp.label -> Tr.stm

  (* infix op have to be declared in every module *)
  infixr 3 </ fun x </ f = f x (* Right application *)
  infix 1 >/ val op>/ = op</ (* Left pipe *)

  val dummy_exp = Nx(Tr.EXP(Tr.CONST(0)))
  
  val nilkw = (Ex (Tr.CONST 0))
                  
  fun cond_stm (genstm: Tp.label * Tp.label -> Tr.stm) r t f =
    Tr.seq[Tr.MOVE(Tr.TEMP r, Tr.CONST 1),
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

  fun unCx (Ex (Tr.CONST(0))) = (fn (t: Tp.label, f: Tp.label) => Tr.JUMP(Tr.NAME(f), [f]))
      | unCx (Ex (Tr.CONST(_))) = (fn (t: Tp.label, f: Tp.label) => Tr.JUMP(Tr.NAME(t), [t]))
      | unCx (Ex e) = (fn (t: Tp.label, f: Tp.label) => Tr.CJUMP(Tr.NE, e, Tr.CONST(0), t, f))
      | unCx (Cx genstm) = genstm
      | unCx (Nx _) = (ErrorMsg.impossible "trying unCx to Nx"; fn (t,f) =>
        Tr.LABEL(t)) (* return a dummy Tree.stm *)


  datatype level = level of level option * Frame.frame * unit ref (* parent_level * unique *)

  datatype access = access of level * Frame.access

  type frag = Frame.frag

  val outermost = level(NONE, Frame.newFrame {name=Tp.newlabel(), escapes=[]} ,ref ())

  val fragments: frag list ref = ref []
  
  fun getResult () =
    ! fragments

  fun newLevel {parent, name, escapes} =
    level(SOME(parent), Frame.newFrame {name=name, escapes=escapes}, ref ())

  fun formals (level(level_opt, frame, uniq)) =
    map (fn formal => access(level(level_opt, frame, uniq), formal)) (Frame.formals frame)

  fun intlit(i) = Ex(Tr.CONST(i))

  fun allocLocal (level(level_opt, frame, uniq)) escape =
  let
    val new_acc = Frame.allocLocal frame escape
    val _ = print("allocLocal: " ^ Frame.access2str new_acc ^ "\n")
  in
    access(level(level_opt, frame, uniq), new_acc)  
  end

  fun procEntryExit (body, level(l, frame, uniq)) =
      let val body' = Frame.procEntryExit1(frame, Tr.MOVE(Tr.TEMP Frame.RV, unEx(body)))
      in
          fragments := Frame.PROC{body = body', frame = frame} :: !fragments
      end

  fun get_static_link (call_level, def_level, fp) =
  let
    fun trace (cur_level, tgt_level) exp=
      (print("trace\n");
        case cur_level of
            level(NONE, _, _) => exp
          | _ => 
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
        )
  in
    (print("tracing\n");
    trace (call_level, def_level) (Tr.TEMP fp)
    )
  end

  fun trace_levels (call_level, def_level, fp) frame_access =
    Ex(Frame.exp frame_access
      (get_static_link(call_level, def_level, fp))
    )

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

  
  fun simpleVar (access(def_lev, fr_acc), call_lev) =
    (print("simpleVar: " ^ Frame.access2str fr_acc ^ "\n");
    trace_levels (call_lev, def_lev, Frame.FP) fr_acc
    )

  fun fieldVar (var: exp, offset: exp) =
    let
      val tmp = Tp.newtemp()
    in
      Ex(Tr.ESEQ(Tr.MOVE(Tr.TEMP(tmp), Tr.BINOP(Tr.PLUS, unEx(var), Tr.BINOP(Tr.MUL, unEx(offset), Tr.CONST(Frame.wordSize)))),
                  Tr.MEM(Tr.TEMP(tmp))))
    end
    
  fun subVar (base_fp: exp, offset: exp) =
    (*Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, unEx(base_fp), unEx(offset))))*)
    let
      val base_tmp = Tp.newtemp()
      val offset_tmp = Tp.newtemp()
      val base_stm = Tr.MOVE(Tr.TEMP(base_tmp), unEx(base_fp))
      val offset_stm = Tr.MOVE(Tr.TEMP(offset_tmp), unEx(offset))
      val get_size = Tr.MEM(Tr.BINOP(Tr.MINUS, Tr.TEMP(base_tmp), Tr.CONST(Frame.wordSize)))
      val t_lb = Tp.newlabel()
      val f_lb = Tp.newlabel()
      val check_stm = Tr.CJUMP(Tr.LT, Tr.TEMP(offset_tmp), get_size, t_lb, f_lb)
      val msg_stm = Frame.externalCall("tig_print", [unEx (strlit "run-time error: index out of bound\n")]) >/ Tr.EXP
      val exit_stm = Frame.externalCall("exit", [Tr.CONST 1]) >/ Tr.EXP
      val get_exp = Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.TEMP(base_tmp), Tr.BINOP(Tr.MUL, Tr.TEMP(offset_tmp), Tr.CONST(Frame.wordSize))))
    in
      Ex(Tr.ESEQ(Tr.seq[base_stm,
                        offset_stm,
                        check_stm,
                        Tr.LABEL(f_lb),
                        msg_stm,
                        exit_stm,
                        Tr.LABEL(t_lb)],
                get_exp))
    end

  fun assign (vExp, eExp) = Nx(Tr.MOVE(unEx vExp, unEx eExp))

  fun createArray (init_exp, size_exp) =
    let
      val tmp = Tp.newtemp()
  		val actual_size = Tr.BINOP(Tr.PLUS, unEx(size_exp), Tr.CONST(1)) (*actual size = array size + 1*)
    in
      (*Ex (Frame.externalCall("tig_initArray", [unEx size_exp, unEx init_exp]))*)
      Ex(Tr.ESEQ(Tr.seq[Tr.MOVE(Tr.TEMP(tmp), Frame.externalCall("tig_initArray", [actual_size, unEx init_exp])),
                        Tr.MOVE(Tr.MEM(Tr.TEMP(tmp)), unEx(size_exp)), (*save size for bounds checking*)
                        Tr.MOVE(Tr.TEMP(tmp), Tr.BINOP(Tr.PLUS, Tr.TEMP(tmp), Tr.CONST(Frame.wordSize)))], (*actual array starts one address higher*)
                  Tr.TEMP(tmp)))
    end
      

  fun seqexp [] = Ex(Tr.CONST(0))
    | seqexp exp_list =
    let
      val tail::body =  exp_list
      val stm_list = map unNx (List.rev body)
    in
      Ex(Tr.ESEQ(Tr.seq stm_list, unEx tail))
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
    Ex(Tr.ESEQ(Tr.seq (head::(List.rev nodes)), Tr.TEMP(base)))
  end

  fun letexp (dec_exps, body_exp) = 
    Ex(Tr.ESEQ(Tr.seq (map unNx dec_exps), unEx body_exp))

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

  fun strOp(oper, lexp, rexp) = 
    case oper of A.EqOp =>
      Ex(Frame.externalCall("tig_stringEqual", [unEx lexp, unEx rexp]))
    | A.NeqOp =>
      Ex(Frame.externalCall("tig_stringNotEqual", [unEx lexp, unEx rexp]))
    | A.PlusOp =>
      Ex(Frame.externalCall("tig_concat", [unEx lexp, unEx rexp]))
    | A.LtOp =>
      Ex(Frame.externalCall("tig_stringLT", [unEx lexp, unEx rexp]))
    | A.GtOp =>
      Ex(Frame.externalCall("tig_stringGT", [unEx lexp, unEx rexp]))
    | _ => (ErrorMsg.impossible "strOp not support"; dummy_exp)
    

  fun whileExp (test, body, joinLabel) =
      let
        val testLabel = Tp.newlabel()
        val bodyLabel = Tp.newlabel()
      in
        Nx (Tr.seq[Tr.LABEL(testLabel),
                unCx(test) (bodyLabel, joinLabel),
                Tr.LABEL(bodyLabel),
                unNx(body),
                Tr.JUMP(Tr.NAME(testLabel), [testLabel]),
                Tr.LABEL(joinLabel)])
      end

  fun breakExp bL = Nx(Tr.JUMP(Tr.NAME(bL), [bL]))

  fun ifExp (cond, thenExp) =
      let
        val cond = unCx(cond)
        val thenLabel = Tp.newlabel()
        val joinLabel = Tp.newlabel()
      in
        Nx (Tr.seq[cond (thenLabel, joinLabel),
                    Tr.LABEL(thenLabel),
                    unNx(thenExp),
                    Tr.LABEL(joinLabel)])
      end

  fun ifelseExp (cond, thenExp, elseExp) =
      let
        val cond = unCx(cond)
        val r = Tp.newtemp()
        val thenLabel = Tp.newlabel()
        val elseLabel = Tp.newlabel()
        val joinLabel = Tp.newlabel()
      in
        Ex (Tr.ESEQ(Tr.seq[(cond) (thenLabel, elseLabel),
                    Tr.LABEL(thenLabel),
                    Tr.MOVE(Tr.TEMP(r), unEx(thenExp)),
                    Tr.JUMP(Tr.NAME(joinLabel), [joinLabel]),
                    Tr.LABEL(elseLabel),
                    Tr.MOVE(Tr.TEMP(r), unEx(elseExp)),
                    Tr.LABEL(joinLabel)],
            Tr.TEMP(r)))
      end

  fun forExp (access(val_level, var_access), lo, hi, body, joinLabel) =
      let
        val bodyLabel = Tp.newlabel()
        val forLabel = Tp.newlabel()
        val var_ex = Frame.exp var_access (Tree.TEMP Frame.FP)
        val hi_ex = unEx hi
        val lo_ex = unEx lo
      in
        Nx (Tr.seq[Tr.MOVE(var_ex, lo_ex),
                Tr.CJUMP(Tr.LE, var_ex, hi_ex, bodyLabel, joinLabel),
                Tr.LABEL(bodyLabel),
                unNx(body),
                Tr.CJUMP(Tr.LT, var_ex, hi_ex, forLabel, joinLabel),
                Tr.LABEL(forLabel),
                Tr.MOVE(var_ex, Tr.BINOP(Tr.PLUS, var_ex, Tr.CONST 1)),
                Tr.JUMP(Tr.NAME(bodyLabel), [bodyLabel]),
                Tr.LABEL(joinLabel)])
      end

  fun call (label, args) =
      let
        (*
          val sl = get_static_link (callLevel, decLevel, Frame.FP)
          *)
          val sl = Tr.TEMP(Frame.FP)
      in
        (*
          Ex (Tr.CALL(Tr.NAME label, sl::(map unEx args)))
        *)
          Ex (Tr.CALL(Tr.NAME label, (map unEx args)))
      end

end
