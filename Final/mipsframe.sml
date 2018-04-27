structure MipsFrame : FRAME =
struct
  structure A = Assem
  val wordSize = 4;

  (* infix op have to be declared in every module *)
  infixr 3 </ fun x </ f = f x (* Right application *)
  infix 1 >/ val op>/ = op</ (* Left pipe *)

  (* k means the head of the shift (the next allocation position) *)
  datatype frame = frame of {name: Temp.label,
                            formals: access list,
                            k: int ref}

  and frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  and access = InFrame of int
             | InReg of Temp.temp

  type register = string

  (* Registers *)
  (* http://www.cs.uwm.edu/classes/cs315/Bacon/Lecture/HTML/ch05s03.html *)

  (* return values from functions *)
  (* val v0 = Temp.newtemp() *)
  val v1 = Temp.newtemp()

  (* first four params *)
  val a0 = Temp.newtemp()
  val a1 = Temp.newtemp()
  val a2 = Temp.newtemp()
  val a3 = Temp.newtemp()

  (* caller save *)
  val t0 = Temp.newtemp()
  val t1 = Temp.newtemp()
  val t2 = Temp.newtemp()
  val t3 = Temp.newtemp()
  val t4 = Temp.newtemp()
  val t5 = Temp.newtemp()
  val t6 = Temp.newtemp()
  val t7 = Temp.newtemp()

  (* callee save *)
  val s0 = Temp.newtemp()
  val s1 = Temp.newtemp()
  val s2 = Temp.newtemp()
  val s3 = Temp.newtemp()
  val s4 = Temp.newtemp()
  val s5 = Temp.newtemp()
  val s6 = Temp.newtemp()
  val s7 = Temp.newtemp()

  (* additional caller save, not preserved across calls *)
  val t8 = Temp.newtemp()
  val t9 = Temp.newtemp()

  val ZERO = Temp.newtemp() (* value 0 *)
  val GP   = Temp.newtemp() (* global pointer *)
  val FP   = Temp.newtemp() (* frame pointer *)
  val SP   = Temp.newtemp() (* stack pointer *)
  val RA   = Temp.newtemp() (* return address *)
  val RV   = Temp.newtemp() (* return value (v0)*)

  val namedRegs = [FP, SP, RA, RV]
  val argRegs = [a0, a1, a2, a3]
  val callersaveRegsExtra = [t8, t9]
  val callersaveRegs = [t0, t1, t2, t3, t4, t5, t6, t7]
  val calleesaveRegs = [s0, s1, s2, s3, s4, s5, s6, s7]

  val user_temps = callersaveRegs @ calleesaveRegs @ callersaveRegsExtra
                           
  val regMapList : (Temp.temp * register) list =
      [(a0,"$a0":register),(a1,"$a1"),(a2,"$a2"),(a3,"$a3"),
       (t0,"$t0"),(t1,"$t1"),(t2,"$t2"),(t3,"$t3"),
       (t4,"$t4"),(t5,"$t5"),(t6,"$t6"),(t7,"$t7"),
       (s0,"$s0"),(s1,"$s1"),(s2,"$s2"),(s3,"$s3"),
       (s4,"$s4"),(s5,"$s5"),(s6,"$s6"),(s7,"$s7"),
       (t8,"$t8"),(t9,"$t9"),(v1,"$v1"),(ZERO,"$zero"),
       (FP,"$fp"),(RV,"$v0"),(SP,"$sp"),(RA,"$ra")]

  val tempMap = foldl (fn ((k, v:register), tbl) => Temp.Map.insert(tbl, k, v))
                      Temp.Map.empty regMapList

  structure RegMap = SplayMapFn 
    (struct
      type ord_key = register
      val compare = String.compare
     end
    )

  val registerMap = foldl (fn ((k, v), tbl) => RegMap.insert(tbl, v, k))
                      RegMap.empty regMapList

  fun temp2reg tp = case Temp.Map.find(tempMap, tp) of 
                  SOME(reg) => reg
                | NONE => Temp.makestring tp
                
  fun reg2temp reg = case RegMap.find(registerMap, reg) of
                          SOME(tp) => tp
                        | NONE => ErrorMsg.impossible ("no such register: " ^
                        reg)

  val user_registers = map (fn tp => let val SOME(e) = Temp.Map.find(tempMap, tp)
                                     in e end) user_temps 

  (*                                   
  fun temp2str tmp=
      case Temp.Map.find(tempMap, tmp) of
          SOME s => (Temp.makestring tmp) ^ "(" ^ s ^ ")"
        | NONE => Temp.makestring tmp
        *)

  fun temp2str tmp=
      case Temp.Map.find(tempMap, tmp) of
          SOME s => s
        | NONE => Temp.makestring tmp

  fun newAccess (k: int ref, escape: bool): access =
    if escape then
      (k := !k - wordSize;
      InFrame(!k + wordSize))
    else
      InReg(Temp.newtemp())


  fun newFrame {name: Temp.label, escapes: bool list} =
  let
    val k = ref 0;
    val formals = map 
      (fn escape => newAccess(k, escape))
      escapes
  in
    frame {name=name, formals=formals, k=k}
  end

  fun name (frame{name, formals, k}) = name

  fun formals (frame{name, formals, k}) = formals

  fun allocLocal (frame{name, formals, k}) escape = newAccess(k, escape) 

  (* given Frame.access and Tree.TEMP(Frame.FP), return a Tree.exp *)
  fun exp (InFrame(k)) fp = 
    Tree.MEM(Tree.BINOP(Tree.MINUS, fp, Tree.CONST(k)))
    | exp (InReg(t: Temp.temp)) _ = Tree.TEMP(t)
  
  fun externalCall (name, args) =
      Tree.CALL(Tree.NAME(Temp.namedlabel name), args)

  fun procEntryExit1 (funFrame: frame, body: Tree.stm) =
      let val frame{name, formals, k} = funFrame
          (* load incoming arguments *)
          (* formarls = [sl, arg0, arg1, ..., arg4_inframe, ...] *)
          val param_len = List.length formals
          val incoming_arguments = if param_len <= 4 then 
            List.take (argRegs, param_len) >/ map (fn r => Tree.TEMP r) else
              (argRegs >/ map (fn r => Tree.TEMP r)) @ 
              List.tabulate(param_len-4, (fn i => Tree.MEM(Tree.BINOP(Tree.PLUS,
              Tree.TEMP FP, Tree.CONST ((i+5)*wordSize)))))
          val load_arguments = (formals, incoming_arguments)
                    >/ ListPair.map (fn (local_arg, income_arg) => 
                        Tree.MOVE(exp local_arg (Tree.TEMP FP), income_arg))

          (* save/restore $ra, callee-save in frame *)
          val prs = RA::calleesaveRegs
                        >/ map (fn r => (allocLocal funFrame true, r))
          val saveInstrs = prs
                               >/ map (fn (a, r) => Tree.MOVE(exp a (Tree.TEMP FP), Tree.TEMP r))
          val restoreInstrs = prs
                                  >/ List.rev
                                  >/ map (fn (a, r) => (Tree.MOVE(Tree.TEMP r, exp a (Tree.TEMP FP))))
      in
        (*
          Tree.SEQ (Tree.LABEL name, load_arguments @ saveInstrs @ [body] @
          restoreInstrs >/ Tree.seq)
          *)
          load_arguments @ saveInstrs @ [body] @
          restoreInstrs >/ Tree.seq
      end

  fun procEntryExit2 (funFrame, bodyInstrs) =
      bodyInstrs @ [A.OPER{assem = "",
                     src = [ZERO, RA, SP] @ calleesaveRegs,
                     dst = [],
                     jump = SOME []}]

  fun procEntryExit3 (frame{name=name, formals=params, k=k}, bodyInstrs) =
      let
          (* allocate space for:
           * local variables
           * saved registers
           * (move to caller) all arguments
           * (move to caller) the static link
          val offset = !k + (List.length formals) + 1
          *)
          val offset = ~(!k)
      in
          
      {
        prolog = "#PROCEDURE " ^ Symbol.name name ^ "\n"
                 ^ ".text\n"
                 ^ Symbol.name name ^ ":\n"
                 ^ "sw $fp, 0$(sp)\n" (* save old FP *)
                 ^ "move $fp $sp\n" (* set current FP to the old SP*)
                 ^ "addi $sp, $sp, -" ^ Int.toString(offset) ^ "\n", (* make the new SP *)
        body = bodyInstrs,
        epilog = "move $sp, $fp\n" (* restore the old SP *)
                 ^ "lw $fp, 0($sp)\n" (* restore the old FP *)
                 ^ "jr $ra\n" ^ (* jump to return address *)
        "#END " ^ (Symbol.name name) ^ "\n"
      }
      end

  fun string (lbl, str) = 
    ".data\n" ^
    (Symbol.name lbl) ^ " .asciiz \"" ^ str ^ "\"\n"
end
