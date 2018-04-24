signature FRAME =
sig 
  type frame
  type access
  type register
  val newFrame : {name: Temp.label,
                escapes: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  
  val FP: Temp.temp
  val SP: Temp.temp
  val RV: Temp.temp (* for return value *)
  val RA: Temp.temp
  val namedRegs: Temp.temp list
  val argRegs: Temp.temp list
  val callersaveRegsExtra: Temp.temp list
  val callersaveRegs: Temp.temp list
  val calleesaveRegs: Temp.temp list
  val user_registers: register list

  val wordSize: int
  val exp : access -> Tree.exp -> Tree.exp
  
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val string: Temp.label * String.string -> String.string

  val externalCall: string * Tree.exp list -> Tree.exp
  
  val procEntryExit1 : frame * Tree.stm -> Tree.stm
  val procEntryExit2: frame * Assem.instr list -> Assem.instr list
  val procEntryExit3: frame * Assem.instr list ->
                      { prolog:string, body:Assem.instr list, epilog:string }
  structure RegMap : ORD_MAP sharing type RegMap.Key.ord_key = register
  val tempMap: register Temp.map
  val registerMap: Temp.temp RegMap.map
  val temp2str: Temp.temp -> string
  val temp2reg: Temp.temp -> register
  val reg2temp: register -> Temp.temp
end
