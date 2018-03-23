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
            SOME(d', esc) => if d > d' then esc := true else ())
      | A.FieldVar(var, _, _) => traverseVar(env, d, var)
      | A.SubscriptVar(var, _, _) => traverseVar(env, d, var)

and traverseExp(env: secEnv, d: depth, s: A.exp): unit =
    let trvsExp(exp: A.exp) =>
        traverseExp(env, d, left)
    in
    case s of
        A.VarExp(var) => traverseVar(env, d, var)
     | A.NilExp => ()
     | A.IntExp => ()
     | A.StringExp => ()
     | A.CallExp({args, ...}) => (map (fn exp => trvsExp(exp)) args; ())
     | A.OpExp{left, right, ...} => (trvsExp(exp); trvsExp(exp))
     | A.RecordExp{fields, ...} => ((map (fn (_, exp, _) => trvsExp(exp)) fields); ())
     | A.SeqExp(exps) => (map (fn(exp, _) => trvsExp(exp)); ())
     | A.AssignExp{exp, ...} => trvsExp(exp)
     | A.IfExp{test, then', else', ...} => (trvsExp(test); trvsExp(then');
                                            case else' of
                                                SOME exp => trvsExp(exp)
                                              | NONE => ())
     | A.WhileExp{test, body, ...} => (trvsExp(test), trvsExp(body))
     | A.ForExp{var, escape, lo, hi, body, ...} =>
       let val env' = S.enter(env, var, (d, escape))
       in
           escape := false;
           trvsExp(lo);
           trvsExp(hi);
           traverseExp(env', d, body)
       end
     | A.BreakExp => ()
     | A.LetExp{decs, body, ...} =>
       let env' = traverseDec(env, d, s)
       in
           traverseExp(env', d, body)
       end
     | A.ArrayExp{size, init, ...} => (trvsExp(size); trvsExp(init))
    end
and traverseDec(env: escEnv, d: depth, s: A.dec)
