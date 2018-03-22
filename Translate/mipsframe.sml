structure MipsFrame : FRAME =
struct
  (* k means the head of the shift (the next allocation position) *)
  datatype frame = frame of {name: Temp.label,
                            formals: access list,
                            k: int ref}

  datatype access = InFrame of int
                  | InReg of Temp.temp

  fun newAccess (k: int ref, escape: bool): access =
    if escape then
      (k := !k - wordSzie;
      InFrame(k + wordSize))
    else
      InReg(Temp.newtemp())


  fun newFrame {name: Temp.lable, escapes: bool list} =
  let
    val k = ref 0;
    val formals = map 
      (fn escape => newAccess(k, escape))
      escapes
  in
    frame {name=name, formals=formals, k=k}
  end

  fun name (frame{name, formals}) = name

  fun formals (frame{name_, formals_}) = formals_

  fun allocLocal (frame{name, formals, k}) escape = newAccess(k, escape) 

  val FP = Temp.newtemp()

  val wordSize = 4;

  (* given Frame.access and Tree.TEMP(Frame.FP), return a Tree.exp *)
  fun exp (InFrame(k)) (Tree.TEMP(fp:Temp.temp)) = 
    Tree.MEM(Tree.BINOP(Tree.PLUS, Temp.TEMP(fp), Tree.CONST(k)))
    | exp (InReg(t: Temp.temp)) _ = Tree.TEMP(t)
  
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  fun externalCall (name, args) =
      Tree.CALL(T.NAME(Temp.namedlabel name), args)

  fun procEntryExit1 (frame: frame, body: Tree.stm) = body

end
