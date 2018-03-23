structure MipsFrame : FRAME =
struct
  val FP = Temp.newtemp()
  val RV = Temp.newtemp()

  val wordSize = 4;

  (* k means the head of the shift (the next allocation position) *)
  datatype frame = frame of {name: Temp.label,
                            formals: access list,
                            k: int ref}

  and access = InFrame of int
                  | InReg of Temp.temp

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
  
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  fun externalCall (name, args) =
      Tree.CALL(Tree.NAME(Temp.namedlabel name), args)

  fun procEntryExit1 (funFrame: frame, body: Tree.stm) =
      let val frame{name, ...} = funFrame
      in
          Tree.SEQ (Tree.LABEL name, body)
      end

end
