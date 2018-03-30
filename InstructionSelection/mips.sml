structure Mips: CODEGEN =
struct
structure Frame = MipsFrame
structure T = Tree
structure F = Frame
structure A = Assem

fun int x = if x >= 0 then Int.toString x else "-" ^ (Int.toString (~x))


fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
let
  val ilist = ref (nil: A.instr list)
  fun emit x = ilist := x :: !ilist
  fun result (gen) = let val t = Temp.newtemp() in gen t; t end

  (* calling a function will trash a particular set of registers: *)
  (*   - args: they are defined to pass parameters; *)
  (*   - callersaves: they may be redefined inside the call; *)
  (*   - RA: it will be overwritten for function return. *)
  (*   - RV: it will be overwritten for function return. *)
  val calldefs = Frame.RA :: (Frame.RV :: Frame.argRegs) @ Frame.callersaveRegs

  fun oper2jump oper =
    case oper of
      T.EQ => "beq"
      | T.NE => "bne"
      | T.LT => "blt"
      | T.GT => "bgt"
      | T.LE => "ble"
      | T.GE => "bge"
      (* unsupport to unsigned condition *)
      | T.ULT => "blt"
      | T.UGT => "bgt"
      | T.ULE => "ble"
      | T.UGE => "bge"
      

  (* generate assembly *)
  fun gs oper =
  let
    fun dtsh shamt = " $rd, $rt, " ^ (shamt) ^ "\n"
    val dst = " $rd, $rs, $rt\n"
    val st = " $rs, $rt\n"
    val d = " $rd\n"
    val s = " $rs\n"
    fun sti lbl = " $rs, $rt, " ^ (lbl) ^ "\n"
    fun tsi imm = " $rt, $rs, " ^ (imm) ^ "\n"
    fun tis imm = " $rt, " ^ (imm) ^"($rs)\n"
    fun  si lbl = " $rs, " ^ (lbl) ^ "\n" 
    fun  ti imm = " $rt, " ^ (imm) ^ "\n"
    fun a addr = " " ^ (addr) ^ "\n"
  in
    case oper of 
       ("sll" | "srl" | "sra")
       => (fn shamt => oper ^ (dtsh shamt))
       | ("add" | "addu" | "sub" | "subu" 
       | "and" | "or" | "xor" | "nor"
       | "slt" | "sltu"
       | "sllv" | "srlv" | "srav")
       => (fn (_) => oper ^ dst)
       | ("mult" | "multu" | "div" | "divu")
       => (fn (_) => oper ^ st)
       | ("mfhi" | "mflo" )
       => (fn (_) => oper ^ d)
       | ("jr" | "jalr" | "mthi" | "mtlo")
       => (fn (_) => oper ^ s)
       | ("beq" | "bne" | "blt" | "ble" | "bgt" | "bge")
       => (fn lbl => oper ^ (sti lbl))
       | ("addi" | "addiu" | "slti" | "sltiu"
       | "andi" | "ori" | "xori")
       => (fn imm => oper ^ (tsi imm))
       | ("lb" | "lh" | "lw" | "lbu" | "lhu"
       | "sb" | "sh" | "sw")
       => (fn imm => oper ^ (tis imm))
       | ("blez" | "bgtz")
       => (fn imm => oper ^ (si imm))
       | ("lui")
       => (fn imm => oper ^ (ti imm))
       | ("j" | "jal")
       => (fn addr => oper ^ (a addr))
       | _
       => (fn (_) => "unmatched " ^ oper)
  end

  (* emit wrapper on A.OPER *)
  fun era (assem, src, dst, jump) =
    emit(A.OPER{assem=assem, src=src, dst=dst, jump=jump})

  fun munchExp ((T.BINOP(T.PLUS, e1, T.CONST(i)))
    | (T.BINOP(T.PLUS, T.CONST(i), e1))) =
    result(fn r => era((gs "addi")(int i), [munchExp e1], [r], NONE))
    | munchExp (T.BINOP(T.PLUS, e1, e2)) =
    result(fn r => era((gs "add")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.MINUS, e1, T.CONST(i))) =
    result(fn r => era((gs "addi")(int (~i)), [munchExp e1], [r], NONE))
    | munchExp (T.BINOP(T.MINUS, e1, e2)) =
    result(fn r => era((gs "sub")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.MUL, e1, e2)) =
    result(fn r => era((gs "mul")(""), [munchExp e1, munchExp e2],
    [r], NONE))
    | munchExp (T.BINOP(T.DIV, e1, e2)) =
    result(fn r => era(((gs "div")("") ^ (gs "mflo")("")), [munchExp e1, munchExp e2],
    [r], NONE))
    | munchExp (T.BINOP(T.AND, e1, e2)) = 
    result(fn r => era((gs "and")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.OR, e1, e2)) = 
    result(fn r => era((gs "or")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.LSHIFT, e1, e2)) = 
    result(fn r => era((gs "sllv")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.RSHIFT, e1, e2)) = 
    result(fn r => era((gs "srlv")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.ARSHIFT, e1, e2)) = 
    result(fn r => era((gs "srav")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.XOR, e1, e2)) = 
    result(fn r => era((gs "xor")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.CONST i) =
    result(fn r => era("addi $rd, $zero, " ^ (int i) ^ "\n", [], [r], NONE))
  	| munchExp (T.TEMP t) = t
  	| munchExp (T.NAME n) =
    result(fn r => era((Symbol.name n) ^ ":\n", [], [], NONE))

    | munchExp ((T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))
  	           | (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)))) =
      result(fn r => era(
                        (gs "lw" (int i)),
                        [munchExp e1],
                        [r],
                        NONE
            ))
  	| munchExp (T.MEM(T.CONST i)) =
      result(fn r => era(
                        "lw $rt " ^ int i ^ "($r0)\n",
			            [],
			            [r],
                        NONE))
 	| munchExp (T.MEM(e1)) =
      result(fn r => era(
                        (gs "lw" ""),
                        [munchExp e1],
                        [r],
                        NONE
            ))
    | munchExp (T.CALL(e, args)) =
      (
        era((gs "jalr" ""), munchExp(e)::munchArgs(0, args), calldefs, NONE);
        Frame.RV
      )
    (*
    | munchExp (T.CALL(_, _)) = ErrorMsg.impossible "Function call exp format error"
    *)
    | munchExp (T.ESEQ(_, _)) = ErrorMsg.impossible "Error, ESEQ should not appear in Tree linearization"

  and munchStm (T.SEQ(a,b)) = (munchStm a; munchStm b)
    | munchStm (T.CJUMP(oper, e1, e2, l1, l2)) =
    era(gs (oper2jump (oper)) (Symbol.name l1), [munchExp e1, munchExp e2], [],
    SOME([l1, l2]))
    | munchStm (T.JUMP(T.NAME label, labelList)) =
    era(gs "j" (Symbol.name label), [], [], SOME(labelList))
    | munchStm (T.JUMP(e1, labelList)) =
    era(gs "jr" "", [munchExp e1], [], SOME(labelList))
    | munchStm ((T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2))
    | (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2))) =
    era((gs "sw")(int i), [munchExp e2], [], NONE)
    | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
    era("sw $rt, " ^ (int i) ^ "($zero)\n", [munchExp e2], [], NONE)
    | munchStm (T.MOVE(T.MEM(e1), e2)) =
    era("sw $rt, 0($rs)\n", [munchExp e2], [], NONE)
    | munchStm (T.MOVE(T.TEMP t, T.CONST i)) =
    era((gs "addi" (int i)) , [], [t], NONE)
    | munchStm (T.MOVE(T.TEMP t, e2)) =
    era("add $rd $rs $zero\n" , [munchExp e2], [t], NONE)
  	| munchStm (T.LABEL lab) =
    era((Symbol.name lab) ^ ":\n", [], [], NONE)
    (* return value of call isn't needed *)
    | munchStm (T.EXP(T.CALL(e, args))) =
      era(
          (gs "jalr" ""),
          munchExp(e)::munchArgs(0, args),
          calldefs,
          NONE
      )
    | munchStm _ =
		emit(A.OPER{assem="",
					src=[],
					dst=[], jump=NONE})

  and munchArgs (i, []) = []
    | munchArgs (i, arg::tl) =
      let
          val len = List.length Frame.argRegs
          val dstTemp = ref F.FP;
          (* fist arg is static link *)
          val dst = if (i > 0 andalso i < len + 1)
                    then (dstTemp := List.nth(Frame.argRegs, i - 1); T.TEMP(!dstTemp))
                    else (munchStm(T.MOVE(T.TEMP(F.SP), T.BINOP(T.MINUS, T.TEMP(F.SP), T.CONST Frame.wordSize))); T.MEM(T.TEMP(F.SP)))
          val _ = munchStm(T.MOVE(dst, arg))
      in
          if(i < len + 1) then
              !dstTemp::munchArgs(i+1, tl)
          else
              []
      end
in
  munchStm stm;
  rev(!ilist)
end


end
