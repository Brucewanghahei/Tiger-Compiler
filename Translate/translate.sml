structure Translate : TRANSLATE =
struct
  structure frame = MipsFrame
  structure Tr = Tree
  structure Tp = Temp

  datatype exp = Ex of Tr.exp
               | Nx of Tr.stm
               | Cx of Tp.label * Tp.label -> Tr.stm

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

  fun newLevel {parent, name, escapes} =
    level(parent, Frame.newFrame(name, escapes), ref ())

  fun formals (level(level_opt, frame, uniq)) =
    Frame.formals(frame)

  fun allocLocal (level(level_opt, frame, uniq)) escape =
    access(level(level_opt, frame, uniq), Frame.allocLocal(frame, escape))

end
