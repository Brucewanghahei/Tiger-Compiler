structure Mips:> CODEGEN =
struct
structure Frame = MipsFrame
structure T = Tree

fun int x = Int.toString x

structure A = Assem

fun codegen (frame) (stm: Tree.stm) : Aseem.instr list =
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
      | T.ULT => "bult"
      | T.UGT => "bugt"
      | T.ULE => "bule"
      | T.UGE => "buge"
      
  fun munchStm (T.SEQ(a,b)) = (munchStm a; munchStm b)
  	| munchStm ((T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i), e2)))
  	| (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1), e2)))) =
		emit(A.OPER{assem="STORE M['s0+" ^ int i ^ "] <- 's1\n",
					src=[munchExp e1, munchExp e2],
					dst=[], jump=NONE})
  	| munchStm (T.MOVE(T.MEM(e1), T.MEM(e2))) =
		emit(A.OPER{assem="MOVE M['s0] <- M['s1]\n",
					src=[munchExp e1, munchExp e2],
					dst=[], jump=NONE})
  	| munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
		emit(A.OPER{assem="STORE M[r0+" ^ int i ^ "] <- 's0\n",
					src=[munchExp e2],
					dst=[], jump=NONE})
  	| munchStm (T.MOVE(T.MEM(e1), e2)) =
	  emit(A.OPER{assem="STORE M['s0] <- 's1\n",
				  src=[munchExp e1, munchExp e2],
				  dst=[], jump=NONE})
  	| munchStm (T.MOVE(T.TEMP i, e2)) =
	  emit(A.OPER{assem="ADD 'd0 <- 's0 + r0\n",
				  src=[munchExp e2],
				  dst=[i], jump=NONE})
  	| munchStm (T.LABEL lab) =
	  emit(A.OPER{assem=lab ^ ":\n",
				  lab=lab})
    | munchStm (T.CJUMP(oper, e1, e2, l1, l2)) =
      emit(A.OPER{assem=oper2jump (oper) ^ " 'd0, 's0, 's1\n",
                  src=[munchExp e1, munchExp e2],
                  dst=[], jum=SOME([trueLabel, falseLabel])})
    | munchStm (T.JUMP(e1, labelList)) =
      emit(A.OPER{assem="jr 'j0\n",
          src=[munchExp e1],
          dst=[], jump=SOME(labelList)})
    | munchStm (T.JUMP(T.NAME label, labelList)) =
      emit(A.OPER{assem="jr 'j0\n",
          src=[munchExp e1],
          dst=[], jump=SOME([label])})
    (* return value of call isn't needed *)
    | munchStm (T.EXP(T.CALL(e, args))) =
      emit(A.OPER
               {assem = "jalr 's0\n",
                src = munchExp(e)::munchArgs(0, args),
                dst = calldefs,
                jump = NONE})
    | munchStm _ =
		emit(A.OPER{assem="",
					src=[],
					dst=[], jump=NONE})


  and munchExp ((T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))
  	| (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)))) =
	   result(fn r => emit(A.OPER
			 {assem="LOAD 'd0 <- M['s0+" ^ int i ^ "]\n",
			 src=[munchExp e1],
			 dst=[r], jump=NONE}))
  	| munchExp (T.MEM(T.CONST i)) =
	   result(fn r => emit(A.OPER
			 {assem="LOAD 'd0 <- M[r0+" ^ int i ^ "]\n",
			 src=[],
			 dst=[r], jump=NONE}))
 	| munchExp (T.MEM(e1)) =
	   result(fn r => emit(A.OPER
			 {assem="LOAD 'd0 <- M['s0+0]\n",
			 src=[munchExp e1],
			 dst=[r], jump=NONE}))
  	| munchExp ((T.BINOP(T.PLUS, e1, T.CONST i))
  	| (T.BINOP(T.PLUS, T.CONST i, e1))) =
	   result(fn r => emit(A.OPER
			 {assem="ADDI 'd0 <- 's0+" ^ int i ^ "\n",
			 src=[munchExp e1],
			 dst=[r], jump=NONE}))
  	| munchExp (T.CONST i) =
	   result(fn r => emit(A.OPER
			 {assem="ADDI 'd0 <- r0+" ^ int i ^ "\n",
			 src=[],
			 dst=[r], jump=NONE}))
  	| munchExp (T.BINOP(T.PLUS, e1, e2)) =
	   result(fn r => emit(A.OPER
			 {assem="ADD 'd0 <- 's0+'s1\n",
			 src=[munchExp e1, munchExp e2],
			 dst=[r], jump=NONE}))
  	| munchExp (T.TEMP t) = t
  	| munchExp (T.NAME n) =
      result(fn r => emit(A.OPER
        {assem="ADDI 'd0 <- r0+" ^ (Symbol.name n) ^ "\n",,
        src=[],
        dst=[r], jump=NONE}))
    | munchExp (T.CALL(e, args)) =
      result(fn r => emit(A.OPER
                              {assem = "jalr 's0\n",
                               src = munchExp(e)::munchArgs(0, args),
                               dst = calldefs,
                               jump = NONE}))
    | munchExp (T.CALL(_, _)) = ErrorMsg.impossible "Function call exp format error"
      
    | munchExp (T.ESEQ(_, _)) = ErrorMsg.impossible "Error, ESEQ should not appear in Tree linearization"
    | munchExp _ =
	   result(fn r => emit(A.OPER
			 {assem="",
			 src=[],
			 dst=[r], jump=NONE}))

  and munchArgs (i, []) = []
    | munchArgs (i, arg::tl) =
      let
          val len = List.length Frame.argRegs
          val dstTemp = ref F.FP;
          (* fist arg is static link *)
          val dst = if (i > 0 andalso i < len + 1)
                    then (dstTemp := List.nth(Frame.argRegs, i - 1); T.TEMP(!dstTemp))
                    else (munchStm(T.MOVE(T.TEMP(F.SP), T.MINUS(T.PLUS, T.TEMP(F.SP), T.CONST Frame.wordSize))); T.MEM(T.TEMP(F.SP)))
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
