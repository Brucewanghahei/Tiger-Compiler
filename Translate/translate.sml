structure Translate : TRANSLATE =
struct
  structure frame = MipsFrame

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm

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
