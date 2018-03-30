structure MipsFrame : FRAME =
struct
  structure A = Assem
  val wordSize = 4;

  (* k means the head of the shift (the next allocation position) *)
  datatype frame = frame of {name: Temp.label,
                            formals: access list,
                            k: int ref}

  and frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  and access = InFrame of int
             | InReg of Temp.temp

  and register = string

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
  val callersaveRegs = [t0, t1, t2, t3, t4, t5, t6, t7]
  val calleesaveRegs = [s0, s1, s2, s3, s4, s5, s6, s7]
                           
  val regMapList =
      [(a0,"$a0"),(a1,"$a1"),(a2,"$a2"),(a3,"$a3"),
       (t0,"$t0"),(t1,"$t1"),(t2,"$t2"),(t3,"$t3"),
       (t4,"$t4"),(t5,"$t5"),(t6,"$t6"),(t7,"$t7"),
       (v0,"$v0"),(v1,"$v1"),(t8,"$t8"),(t9,"$t9"),
       (s0,"$s0"),(s1,"$s1"),(s2,"$s2"),(s3,"$s3"),
       (s4,"$s4"),(s5,"$s5"),(s6,"$s6"),(s7,"$s7"),
       (FP,"$fp"),(RV,"$v0"),(SP,"$sp"),(RA,"$ra"),
       (ZERO,"$zero")]

  val tempMap = foldl (fn ((k, v), tbl) => Temp.Map.insert(tbl, k, v))
                      Temp.Map.empty regMapList

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
    Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(k)))
    | exp (InReg(t: Temp.temp)) _ = Tree.TEMP(t)
  
  fun externalCall (name, args) =
      Tree.CALL(Tree.NAME(Temp.namedlabel name), args)

  fun procEntryExit1 (funFrame: frame, body: Tree.stm) =
      let val frame{name, ...} = funFrame
      in
          Tree.SEQ (Tree.LABEL name, body)
      end

  fun procEntryExit2 (funFrame, bodyInstrs) =
      bodyInstrs @ [A.OPER{assem = "",
                     src = [ZERO, RA, SP] @ calleesaveRegs,
                     dst = [],
                     jump = SOME []}]

  fun procEntryExit3 (frame{name=name, formals=params, k=k}, bodyInstrs) =
      {
        prolog = "PROCEDURE " ^ Symbol.name name ^ "\n",
        body = bodyInstrs,
        epilog = "END " ^ (Symbol.name name) ^ "\n"
      }

  fun string (lbl, str) = (Symbol.name lbl) ^ str
end
