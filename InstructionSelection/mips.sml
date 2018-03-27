structure Mips:
          sig structure Frame: FRAME
              val codegen: Frame.frame -> Tree.stm -> Assem.instr list
          end =
struct
structure Frame = MipsFrame

end
end
