structure Mips: CODEGEN =
struct
structure Frame = MipsFrame
structure T = Tree
structure F = MipsFrame
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
      

  (* generote assembly *)
  fun gs oper =
  let
    (*
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
    fun a addr  = " " ^ (addr) ^ "\n"
    fun da addr = " $rd, " ^ addr  ^ "\n"
      *)
    fun dtsh shamt = " `d0, `s0, " ^ (shamt) ^ "\n"
    val dst = " `d0, `s0, `s1\n"
    val st = " `s0, `s1\n"
    val d = " `d0\n"
    val s = " `s0\n"
    fun sti lbl = " `s0, `s1, " ^ (lbl) ^ "\n"
    fun tsi imm = " `d0, `s0, " ^ (imm) ^ "\n"
    fun dis imm = " `d0, " ^ (imm) ^"(`s0)\n"
    fun tis imm = " `s1, " ^ (imm) ^"(`s0)\n"
    fun  si lbl = " `s0, " ^ (lbl) ^ "\n" 
    fun  ti imm = " `s0, " ^ (imm) ^ "\n"
    fun a addr  = " " ^ (addr) ^ "\n"
    fun da addr = " `d0, " ^ addr  ^ "\n"
  in
    case oper of 
       ("sll" | "srl" | "sra")
       => (fn shamt => oper ^ (dtsh shamt))
       | ("add" | "addu" | "sub" | "subu" | "mul"
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
       | ("lb" | "lh" | "lw" | "lbu" | "lhu")
       => (fn imm => oper ^ (dis imm))
       | ("sb" | "sh" | "sw")
       => (fn imm => oper ^ (tis imm))
       | ("blez" | "bgtz")
       => (fn imm => oper ^ (si imm))
       | ("lui")
       => (fn imm => oper ^ (ti imm))
       | ("j" | "jal")
       => (fn addr => oper ^ (a addr))
       | ("la" | "li")
       => (fn addr => oper ^ (da addr))
       | _
       => (fn (_) => "unmatched " ^ oper)
  end

  (* emit wrapper on A.OPER *)
  fun ero (assem, src, dst, jump) =
    emit(A.OPER{assem=assem, src=src, dst=dst, jump=jump})

  (* emit wrapper on A.LABEL *)
  fun erl (assem, lab) =
    emit(A.LABEL{assem=assem, lab=lab})

  (* emit wrapper on A.MOVE *)
  fun erm (assem, dst, src) =
    emit(A.MOVE{assem=assem, dst=dst, src=src})

  fun munchExp ((T.BINOP(T.PLUS, e1, T.CONST(i)))
    | (T.BINOP(T.PLUS, T.CONST(i), e1))) =
    result(fn r => ero((gs "addi")(int i), [munchExp e1], [r], NONE))
    | munchExp (T.BINOP(T.PLUS, e1, e2)) =
    result(fn r => ero((gs "add")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.MINUS, e1, T.CONST(i))) =
    result(fn r => ero((gs "addi")(int (~i)), [munchExp e1], [r], NONE))
    | munchExp (T.BINOP(T.MINUS, e1, e2)) =
    result(fn r => ero((gs "sub")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.MUL, e1, e2)) =
    result(fn r => ero((gs "mul")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.DIV, e1, e2)) =
    result(fn r => (ero(gs "div" "", [munchExp e1, munchExp e2], [r], NONE);
                    ero(gs "mflo" "", [], [r], NONE)))
    | munchExp (T.BINOP(T.AND, e1, e2)) = 
    result(fn r => ero((gs "and")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.OR, e1, e2)) = 
    result(fn r => ero((gs "or")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.LSHIFT, e1, e2)) = 
    result(fn r => ero((gs "sllv")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.RSHIFT, e1, e2)) = 
    result(fn r => ero((gs "srlv")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.ARSHIFT, e1, e2)) = 
    result(fn r => ero((gs "srav")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.BINOP(T.XOR, e1, e2)) = 
    result(fn r => ero((gs "xor")(""), [munchExp e1, munchExp e2], [r], NONE))
    | munchExp (T.CONST i) =
    result(fn r => ero((gs "li" (int i)), [], [r], NONE))
  	| munchExp (T.TEMP t) = t
  	| munchExp (T.NAME n) =
    result(fn r => ero((gs "la" (Symbol.name n)), [], [r], NONE))

    | munchExp ((T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))
  	           | (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)))) =
      result(fn r => ero(
                        (gs "lw" (int i)),
                        [munchExp e1],
                        [r],
                        NONE
            ))
  	| munchExp (T.MEM(T.CONST i)) =
      result(fn r => ero(
                        "lw `d0, " ^ int i ^ "($zero)\n",
			            [],
			            [r],
                        NONE))
 	| munchExp (T.MEM(e1)) =
      result(fn r => ero(
                        (gs "lw" "0"),
                        [munchExp e1],
                        [r],
                        NONE
            ))
    | munchExp (T.CALL(T.NAME label, args)) =
      (
        ero((gs "jal" (Symbol.name label)), munchArgs(args),
        calldefs, SOME([label]));
        Frame.RV
      )
    (*
    | munchExp (T.CALL(_, _)) = ErrorMsg.impossible "Function call exp format error"
    *)
    | munchExp (T.ESEQ(_, _)) = ErrorMsg.impossible "Error, ESEQ should not appear in Tree linearization"
    | munchExp (_) = ErrorMsg.impossible "Unknown munchExp"

  and munchStm (T.SEQ(a,b)) = (munchStm a; munchStm b)
    | munchStm (T.CJUMP(oper, e1, e2, l1, l2)) =
    ero(gs (oper2jump (oper)) (Symbol.name l1), [munchExp e1, munchExp e2], [],
    SOME([l1, l2]))
    | munchStm (T.JUMP(T.NAME label, labelList)) =
    ero(gs "j" (Symbol.name label), [], [], SOME(labelList))
    | munchStm (T.JUMP(e1, labelList)) =
    ero(gs "jr" "", [munchExp e1], [], SOME(labelList))
    | munchStm ((T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2))
    | (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2))) =
    ero((gs "sw")(int i), [munchExp e1, munchExp e2], [], NONE)
    | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
    ero("sw `s0, " ^ (int i) ^ "($zero)\n", [munchExp e2], [], NONE)
    | munchStm (T.MOVE(T.MEM(e1), e2)) =
    ero("sw `s1, 0(`s0)\n", [munchExp e1, munchExp e2], [], NONE)
    | munchStm (T.MOVE(T.TEMP t, T.CONST i)) =
    ero((gs "li" (int i)) , [], [t], NONE)
    | munchStm (T.MOVE(T.TEMP t, T.BINOP(T.PLUS, T.CONST i, e1))) =
    ero((gs "addi" (int i)), [munchExp e1], [t], NONE)
    | munchStm (T.MOVE(T.TEMP t, T.NAME n)) =
    ero(gs "la" (Symbol.name n), [], [t], NONE)
    | munchStm (T.MOVE(T.TEMP t, e2)) =
      erm("add `d0, `s0, $zero\n", t, munchExp e2)
  	| munchStm (T.LABEL lab) =
    erl((Symbol.name lab) ^ ":\n", lab)
    (* return value of call isn't needed *)
    | munchStm (T.EXP(e1)) =
    ero("", [munchExp e1], [], NONE)
    | munchStm _ =
		emit(A.OPER{assem="non match\n",
					src=[],
					dst=[], jump=NONE})

  and munchArgs (args) =
      (* See Figure 6.2
       * / arg5 /
       * / arg4 / fifth arg
       * /  SL  /
       *)
      let val len = List.length F.argRegs
          fun helper (i, []) = []
            | helper (i, arg::tl) =
              let
                  val dstTemp = if (i < len)
                                then SOME(List.nth(F.argRegs, i))
                                else NONE
                  val dst = case dstTemp of
                                SOME t => t
                              | NONE => let val offset = (i - len + 1) * F.wordSize
                                        in
                                            T.MEM(T.BINOP(T.PLUS, T.CONST offset, T.TEMP(F.SP)))
                                        end
                  val _ = munchStm(T.MOVE(dst, arg))
              in
                  case dstTemp of
                      SOME t => t::helper(i + 1, tl)
                    | NONE => []
              end
      in
          helper(0, args)
      end
in
  munchStm stm;
  rev(!ilist)
end


end
