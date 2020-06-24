module Program.L where

import Prelude hiding (succ)
import Syntax
import Program.Pair
import Program.List
import Program.Num

query = Program typo $ fresh ["t", "gamma"] (call "type_" [V "t", V "gamma", just int])
query1 = Program typo $ fresh ["t"] (call "type_" [V "t", nil, just int])


typo :: [Def]
typo = typeDef : idx

-- De Bruijn encoding for L lang
-- data L = Iconst_ Int
--        | Bconst_ Bool
--        | Var_ Int
--        | Plus_ L L
--        | Mult_ L L
--        | Equal_ L L
--        | Less_ L L
--        | If_ L L L
--        | Let_ L L


just x = C "some" [x]
none = C "none" []

typeDef :: Def
typeDef =
    ( Def "type_" ["term", "gamma", "ttype"]
      (
        fresh ["x", "y", "v", "m", "n", "l", "r", "t", "t1", "bound", "body", "btype", "btype1", "cond", "thn", "els"]
        (
          (term === C "bConst_" [x] &&& ttype === just bool) |||
          (term === C "iConst_" [y] &&& ttype === just int) |||
          (term === C "var_" [v] &&& call "idx" [v, gamma, ttype]) |||
          (term === C "plus_" [m, n] &&& call "type_" [m, gamma, just int] &&& call "type_" [n, gamma, just int] &&& ttype === just int) |||
          (term === C "mult_" [m, n] &&& call "type_" [m, gamma, just int] &&& call "type_" [n, gamma, just int] &&& ttype === just int) |||
          (term === C "equal_" [l, r] &&& call "type_" [l, gamma, t] &&& call "type_" [r, gamma, t] &&& ttype === just bool &&& t === just t1) |||
          (term === C "less_" [l, r] &&& call "type_" [l, gamma, just int] &&& call "type_" [r, gamma, just int] &&& ttype === just bool) |||
          (term === C "let_" [bound, body] &&& call "type_" [bound, gamma, btype] &&& btype === just btype1 &&& call "type_" [body, btype1 % gamma, ttype]) |||
          (term === C "if_" [cond, thn, els] &&& call "type_" [cond, gamma, just bool] &&& call "type_" [thn, gamma, ttype] &&& call "type_" [els, gamma, ttype])
        )
      )
    )
  where
    [term, gamma, ttype, x, y, v, m, n, l, r, t, t1, bound, body, btype, btype1, cond, thn, els] =
      map V ["term", "gamma", "ttype", "x", "y", "v", "m", "n", "l", "r", "t", "t1", "bound", "body", "btype", "btype1", "cond", "thn", "els"]


idx :: [Def]
idx = [idxDef]

idxDef :: Def
idxDef =
    ( Def "idx" ["k", "xs", "v"]
      (
        fresh ["h", "t", "k1"]
        (
          k === zero &&& xs === nil &&& v === none |||
          k === zero &&& xs === h%t &&& v === just h |||
          k === succ k1 &&& xs === h%t &&& call "idx" [k1, t, v]
        )
      )
    )
  where
    [k, xs, v, h, t, k1] = map V ["k", "xs", "v", "h", "t", "k1"]

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
-- type(lt(X,Y), Gamma, boolean) :- type(X,Gamma,integer), type(Y,Gamma,integer).

-- type(let(X,T,B), Gamma, Type) :- type(T,Gamma,Ttype), add(X,Ttype, Gamma, NewGamma), type(B, NewGamma, Type).

-- type(if(C,T,E), Gamma, Type) :- type(C,Gamma,boolean), type(T,Gamma,Type), type(E,Gamma,Type).
