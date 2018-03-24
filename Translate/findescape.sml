structure FindEscape: sig
  val findEscape: Absyn.exp -> unit end =
struct

structure S = Symbol
structure A = Absyn

type depth = int
type escEnv = (depth * bool ref) S.table

fun traverseVar(env: escEnv, d: depth, s: A.var): unit =
    case s of
        A.SimpleVar(sym,_) =>
        (case S.look(env, sym) of
             SOME(d', esc) => if d > d' then esc := true else ()
           | NONE => (ErrorMsg.impossible "should not reach here"; ()))
            
      | A.FieldVar(var, _, _) => traverseVar(env, d, var)
      | A.SubscriptVar(var, _, _) => traverseVar(env, d, var)

and traverseExp(env: escEnv, d: depth, s: A.exp): unit =
    let fun trvsExp(exp: A.exp) = traverseExp(env, d, exp)
    in
    case s of
        A.VarExp(var) => traverseVar(env, d, var)
     | A.NilExp => ()
     | A.IntExp(_) => ()
     | A.StringExp(_) => ()
     | A.CallExp({args, ...}) => (map (fn exp => trvsExp(exp)) args; ())
     | A.OpExp{left, right, ...} => (trvsExp(left); trvsExp(right))
     | A.RecordExp{fields, ...} => ((map (fn (_, exp, _) => trvsExp(exp)) fields); ())
     | A.SeqExp(exps) => (map (fn(exp, _) => trvsExp(exp)); ())
     | A.AssignExp{exp, ...} => trvsExp(exp)
     | A.IfExp{test, then', else', ...} => (trvsExp(test); trvsExp(then');
                                            case else' of
                                                SOME exp => trvsExp(exp)
                                              | NONE => ())
     | A.WhileExp{test, body, ...} => (trvsExp(test); trvsExp(body))
     | A.ForExp{var, escape, lo, hi, body, ...} =>
       let val env' = S.enter(env, var, (d, escape))
       in
           escape := false;
           trvsExp(lo);
           trvsExp(hi);
           traverseExp(env', d, body)
       end
     | A.BreakExp(_) => ()
     | A.LetExp{decs, body, ...} =>
       let val env' = traverseDecs(env, d, decs)
       in
           traverseExp(env', d, body)
       end
     | A.ArrayExp{size, init, ...} => (trvsExp(size); trvsExp(init))
    end
and traverseDecs(env: escEnv, d: depth, s: A.dec list): escEnv =
    let fun trvsDec(dec, env) =
            case dec of
                A.FunctionDec(decs) =>
                (map (fn {params, body, ...} =>
                         let val env' = foldl (fn ({name, escape, ...}, env) => S.enter(env, name, (d+1, escape))) env params
                         in
                             traverseExp(env', d+1, body)
                         end)
                     decs;
                 env)
              | A.VarDec{name, escape, init, ...} => (escape := false; traverseExp(env, d, init); S.enter(env, name, (d, escape)))
              | A.TypeDec(_) => env
    in
        foldl trvsDec env s
    end

and findEscape (prog) = traverseExp(S.empty, 0, prog)
end
