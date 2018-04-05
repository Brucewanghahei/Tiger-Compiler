(* http://mlton.org/InfixingOperators *)
infixr 3 </     fun x </ f = f x                  (* Right application *)
infix 1 >/ val op>/ = op</ (* Left pipe *)
