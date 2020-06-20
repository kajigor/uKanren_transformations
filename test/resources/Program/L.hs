module Program.L where

import Prelude hiding (succ)
import Syntax
import Program.Pair
import Program.List
import Program.Num

query = Program typo $ fresh ["t", "gamma"] (call "type" [V "t", V "gamma", int])

typo :: [Def]
typo = typeDef : assoco

typeDef :: Def
typeDef =
    ( Def "type" ["term", "gamma", "ttype"]
      (
        fresh ["x", "y", "v", "m", "n", "l", "r", "t", "var", "bound", "body", "btype", "cond", "thn", "els"]
        (
          (term === C "bConst" [x] &&& ttype === bool) |||
          (term === C "iConst" [y] &&& ttype === int) |||
          (term === C "var_" [v] &&& call "assoco" [v, gamma, ttype]) |||
          (term === C "plus" [m, n] &&& call "type" [m, gamma, int] &&& call "type" [n, gamma, int] &&& ttype === int) |||
          (term === C "mult" [m, n] &&& call "type" [m, gamma, int] &&& call "type" [n, gamma, int] &&& ttype === int) |||
          (term === C "eq" [l, r] &&& call "type" [l, gamma, t] &&& call "type" [r, gamma, t] &&& ttype === bool) |||
          (term === C "lt" [l, r] &&& call "type" [l, gamma, t] &&& call "type" [r, gamma, t] &&& ttype === bool) |||
          (term === C "let_" [var, bound, body] &&& call "type" [bound, gamma, btype] &&& call "type" [body, pair var btype % gamma, ttype]) |||
          (term === C "if_" [cond, thn, els] &&& call "type" [cond, gamma, bool] &&& call "type" [thn, gamma, ttype] &&& call "type" [els, gamma, ttype])
        )
      )
    )
  where
    [term, gamma, ttype, x, y, v, m, n, l, r, t, var, bound, body, btype, cond, thn, els] =
      map V ["term", "gamma", "ttype", "x", "y", "v", "m", "n", "l", "r", "t", "var", "bound", "body", "btype", "cond", "thn", "els"]


-- (term === C "let_" [var, bound, body] &&& call "type" [bound, gamma, btype] &&& call "addToGamma" [var, btype, gamma, newGamma] &&& call "type" [body, newGamma, ttype]) |||

int = C "integer" []
bool = C "boolean" []

elemo :: [Def]
elemo = [elemoDef]

elemoDef :: Def
elemoDef =
    ( Def "elemo" ["n", "s", "v"]
      (
        fresh ["h", "t", "n1"]
        (
          n === zero &&& s === h % t &&& v === h |||
          n === succ n1 &&& s === h % t &&& call "elemo" [n1, t, v]
        )
      )
    )
    where [n, s, v, h, t, n1] = map V ["n", "s", "v", "h", "t", "n1"]



-- type(bConst(X),Gamma,boolean).
-- type(iConst(X),Gamma,integer).
-- type(var(X),Gamma,Type) :- lookup(X, Gamma, Type).

-- type(plus(X,Y), Gamma, integer) :- type(X,Gamma,integer), type(Y,Gamma,integer).
-- type(mult(X,Y), Gamma, integer) :- type(X,Gamma,integer), type(Y,Gamma,integer).

-- type(eq(X,Y), Gamma, boolean) :- type(X,Gamma,T), type(Y,Gamma,T).
-- type(gt(X,Y), Gamma, boolean) :- type(X,Gamma,T), type(Y,Gamma,T).

-- type(let(X,T,B), Gamma, Type) :- type(T,Gamma,Ttype), add(X,Ttype, Gamma, NewGamma), type(B, NewGamma, Type).

-- type(if(C,T,E), Gamma, Type) :- type(C,Gamma,boolean), type(T,Gamma,Type), type(E,Gamma,Type).
