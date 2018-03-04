use "env.sml";
use "types.sml";
use "symbol.sml";
use "absyn.sml";
use "transalte.sml";
use "errormsg.sml";

signature SEMANT =
sig
  
  type venv
  type tenv
  type expty

  val transVar : venv * tenv * Absyn.var -> expty

  val transExp : venv * tenv * Absyn.exp -> expty

  val transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}

  val transTy : tenv * Absyn.ty -> Types.ty

  val transProg : Absyn.exp -> unit

end

structure Semant :> SEMANT =
struct

  (* alias *)
  structure Err = ErrorMsg
  structure A = Absyn
  structure Ty = Types
  structure E = Env
  structure S = Symbol
  val err = Err.error

  fun assertEq (lhs: 'a, rhs: 'a, eqFun, errCurry, msg) =
      if eqFun(lhs, rhs) then
          errCurry msg
      else
          ()

  (* use T.NIL as dummy return value *)
  fun lookActualType(tenv, symbol, pos) =
      case S.look(tenv, symbol) of
          SOME ty => actual_ty(ty, pos)
        | NONE => (err pos ("Type " ^ s.name symbol ^ "not found"); T.NIL)

  fun actual_ty(ty, pos) =
      case ty of
          T.NAME(s, ref(SOME(t))) => actual_ty(t, pos)
        | T.NAME(s, ref(NONE)) => (err pos ("Type " ^ s.name s ^ "is NONE"); T.NIL)
        | _ => ty

  type venv = Env.enventry Symbol.table
  type tenv = ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  fun checkInt ({exp, ty}, pos) = 
    assertEq (ty, Ty.INT, op =, err pos, "integer required";

  fun transExp(venv, tenv, exp) =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
        (checkInt(trexp left, pos);
         checkInt(trexp right, pos);
         {exp=(), ty=Ty.INT})
      | trexp (A.IntExp int) =
        {exp=(), ty=Ty.INT}
      | trexp (A.LetExp {decs, body, pos}) =
        let
          val {venv=venv', tenv=tenv'} =
          transDec(venv, tenv, decs)
        in
          transExp(venv', tenv', body)
        end
      | trexp (A.SeqExp seq) =
        case seq of
             [(exp, pos)] => trexp(exp)
           | (exp, pos)::tail =>
               (trexp(exp);
                trexp(tail);)
        (* ... *)
    in
      trexp exp
    end

  fun transDec (venv, tenv, dec) =
      let fun trdec (A.VarDec{name, typ = NONE, init, pos}) =
              let val {exp, ty} = transExp(venv, tenv, init)
              in
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = ty}),
                    tenv = tenv
                  }
              end
            | trdec (A.VarDec{name, typ = SOME(s, _), init, pos}) = 
              let val {exp, ty} = transExp(venv, tenv, init)
                  val msgTmpl = "VarDec: type mismatched"
                  val decTy = lookActualType(tenv, s, pos)
              in
                  case ty of
                      T.NIL => case decTy of
                                   T.RECORD => ()
                                 | _ => err pos (msgTmpl ^ " - nil")
                    | T.RECORD =>
                      (* structural equal *)
                      assertEq(#1 ty, #1 decTy,
                               op =,
                               err pos, msgTmpl ^ " - record")
                    | T.ARRAY =>
                      (* structural equal *)
                      assertEq(#1 ty, #1 decTy,
                               op =,
                               err pos, msgTmpl ^ " - array")
                    | _ => assertEq(decTy, ty, op =, err pos, msgTmpl ^ (S.name s))
                ;
                  {
                    venv = S.enter(venv, name, E.VarEntry{ty = decTy}),
                    tenv = tenv
                  }
              end
            | trdec (A.TypeDec(tydecs)) =
              let
                  (* first pass to scan headers*)
                  fun trTyDecHeader (tenv, {name, ty, pos}::tl) =
                      let val tenv' = S.enter(tenv, name, Types.NAME(name, ref NONE))
                      in
                          trTyDecHeader(tenv', tl)
                      end
                    | trTyDecHeader (tenv, nil) =
                      tenv
                  (* second pass to fill body *)
                  fun trTyDecBody (tenv, {name, ty, pos}::tl) =
                      let val nameTy = transTy(tenv, ty)
                      in
                          case S.look(tenv, name) of
                              (s, SOME nameTyRef) =>
                              nameTyRef := nameTy
                            | (s, NONE) =>
                              err pos ("Type" ^ S.name name ^ "not found in header")
                        ;
                          trTyDecBody(tenv, tl)
                      end
                    | trTyDecBody (tenv, nil) = tenv
                  val tenv' = trTyDecHeader(tenv, tydecs)
              in
                  trTyDecBody(tenv', tydecs)
              end
      in
          trdec dec
      end
end
