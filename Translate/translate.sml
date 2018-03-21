structure Translate : TRANSLATE =
struct
  structure frame = MipsFrame
  structure Te = Tree

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

  fun cond_stm (genstm: Temp.label * Temp.label -> Tree.stm) r t f Tree.stm =
    seq[Te.MOVE(Te.TEMP r, Te.CONST 1),
        genstm(t,f),
        Te.LABEL f,
        Te.MOVE(Te.TEMP r, Te.CONST 0),
        Te.LABEL t]

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
        let val r = Temp.newtemp()
            val t = Temp.newlabel() and f = Temp.newlabel()
        in
          Te.ESEQ(cond_stm(genstm r t f),  Te.TEMP r)
        end
    | unEX (Nx s) = Te.ESEQ(s, Te.CONST 0)


  fun unNx (Ex e) = Te.EXP e
    | unNx (Nx stm) = stm
    | unNx (Cx genstm) = Te.EXP(unEx (Cx genstm)) 


  datatype level = level of level option * Frame.frame * unit ref (* parent_level * unique *)

  datatype access = access of level * Frame.access

  val outermost = level(NONE, Frame.newFrame(Temp.newLabel(), []) ,ref ())

  fun newLevel {parent, name, escapes} =
    level(parent, Frame.newFrame(name, escapes), ref ())

  fun formals (level(level_opt, frame, uniq)) =
    Frame.formals(frame)

  fun allocLocal (level(level_opt, frame, uniq)) escape =
    access(level(level_opt, frame, uniq), Frame.allocLocal(frame, escape))

end
