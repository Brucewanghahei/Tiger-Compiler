use "parsetest.sml";
use "semant.sml";

structure Main = 
struct
  val absyn_exp = Parse.parse filename
  fun transProg(absyn_exp) = ()
end
