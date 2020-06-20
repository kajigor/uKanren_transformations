module Program.Typing where

import Prelude hiding (succ)
import Syntax
import Program.Pair
import Program.List
import Program.Num

query = Program typo $ fresh ["t", "gamma", "x", "y"] (call "type" [V "t", V "gamma", C "arrow" [V "x", V "y"]])

typo :: [Def]
typo = typeDef : elemo

typeDef :: Def
typeDef =
    ( Def "type" ["term", "gamma", "ttype"]
      (
        fresh ["x", "y", "v", "vt", "t", "t1", "t2", "t1type", "t2type", "gamma1"]
        (
          (term === C "bConst" [x] &&& ttype === C "boolean" []) |||
          (term === C "iConst" [y] &&& ttype === C "integer" []) |||
          (term === C "var" [v] &&& call "elemo" [v, gamma, ttype]) |||
          (term === C "abs" [v, vt, t] &&& gamma1 === (pair v vt) % gamma &&& call "type" [t, gamma1, ttype]) |||
          (term === C "app" [t1, t2] &&& call "type" [t1, gamma, C "arrow" [t1type, t2type]] &&& call "type" [t2, gamma, t2type])
        )
      )
    )
  where
    [term, gamma, ttype, x, y, v, vt, t, t1, t2, t1type, t2type, gamma1] = map V ["term", "gamma", "ttype", "x", "y", "v", "vt", "t", "t1", "t2", "t1type", "t2type", "gamma1"]


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
-- type(abs(X,Xtype,T), Gamma, arrow(Xtype, Type)) :- add(X,Xtype,Gamma,NewGamma), type(T, NewGamma, Type).
-- type(app(T1, T2), Gamma, T2type) :- type(T1, Gamma, arrow(T1type,T2type)), type(T2,Gamma, T2type).