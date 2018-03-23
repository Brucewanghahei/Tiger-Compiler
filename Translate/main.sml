structure Main = 
struct
  fun compile filename =
  let
    val absyn_exp = Parse.parse filename
    val frag_list = Semant.transProg(absyn_exp) 
    val stm_list = map (fn frag =>
    case frag of 
        Translate.Frame.PROC {body=body, frame=frame} => body
      | Translate.Frame.STRING (string_label, str) => Tree.LABEL(string_label)) frag_list
    val out = TextIO.stdOut
  in
    map (fn stm => Printtree.printtree(out, stm)) stm_list 
  end
end
