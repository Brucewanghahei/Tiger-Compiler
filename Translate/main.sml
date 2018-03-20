structure Main = 
struct
  fun compile filename =
  let
    val absyn_exp = Parse.parse filename
    val _ = Semant.transProg(absyn_exp) 
  in
    ()
  end
end
