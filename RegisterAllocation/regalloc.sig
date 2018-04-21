signature REG_ALLOC =
sig
    structure Frame: FRAME

    type allocation = Frame.register Temp.map

    val alloc: Assem.instr * Frame.frame -> Assem.instr list * allocation
end
