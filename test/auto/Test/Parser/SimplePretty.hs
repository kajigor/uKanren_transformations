module Test.Parser.SimplePretty where

import           Control.Monad       (zipWithM_)
import           Parser.SimplePretty
import           Program
import qualified Program.Num
import qualified Program.Prop
import           Syntax
import           Test.Helper

x, y, z, m, n, h, t :: Term X
[x, y, z, m, n, h, t] = map V ["x", "y", "z", "m", "n", "h", "t"]

v0, v1, v2, v3, v13, v42 :: Term S
[v0, v1, v2, v3, v13, v42] = map V [0, 1, 2, 3, 13, 42]

cons :: Term a -> Term a -> Term a
cons h t = C "Cons" [h, t]

testPretty :: SimplePretty a => a -> String -> Assertion
testPretty = test prettyString

testPrettyWithWidth :: SimplePretty a => Int -> a -> String -> Assertion
testPrettyWithWidth width = test (prettyStringLimitWidth width)

unit_failTerm :: Assertion
unit_failTerm = do
  testPretty (V "") "Var name cannot be empty"
  testPretty (C "" [x, y]) "Constructor name cannot be empty"

unit_prettyVar :: Assertion
unit_prettyVar = do
  zipWithM_ testPretty [x, y, z, m, n] ["x", "y", "z", "m", "n", "h", "t"]
  zipWithM_ testPretty [v0, v1, v2, v3, v13, v42] ["v.0", "v.1", "v.2", "v.3", "v.13", "v.42"]
  testPretty (V "X") "x"
  testPretty (V "Abc") "abc"

unit_prettyCons :: Assertion
unit_prettyCons = do
  testPretty (C "Abc" ([] :: [Term X])) "Abc"
  testPretty (C "abc" ([] :: [Term X])) "Abc"
  testPretty (C "A" [x]) "A x"
  testPretty (C "A" [x, y, z]) "A x y z"
  testPretty (C "Cons" [h, C "Cons" [x, t]]) "Cons h (Cons x t)"

  testPretty (C "a" ([] :: [Term S])) "A"
  testPretty (C "Cons" [v0, C "Cons" [v1, v2]]) "Cons v.0 (Cons v.1 v.2)"

unit_prettyUnify :: Assertion
unit_prettyUnify = do
  testPretty (x :=: y) "x == y"
  testPretty (v13 :=: v42) "v.13 == v.42"
  testPretty (m :=: C "Cons" [h, C "Cons" [x, t]]) "m == Cons h (Cons x t)"
  testPretty (C "Cons" [v0, C "Cons" [v1, v2]] :=: v42) "Cons v.0 (Cons v.1 v.2) == v.42"

unit_prettyFresh :: Assertion
unit_prettyFresh = do
  testPretty (Fresh "x" (x :=: x)) "fresh x in\n  x == x"
  testPretty (Fresh "x" $ Fresh "y" $ Fresh "z" (x :=: y)) "fresh x, y, z in\n  x == y"

-- unit_prettyGoal :: Assertion
-- unit_prettyGoal = do
--   let xIsX = x :=: x
--   testPretty (Conjunction (Disjunction xIsX xIsX []) (Disjunction xIsX xIsX []) []) "(x == x | x == x) & (x == x | x == x)"
--   testPretty (Disjunction (Fresh "h" $ Fresh "t" $ Conjunction (x :=: cons h t) (Fresh "z" (x :=: x)) []) (Fresh "x" (x :=: x)) []) "(fresh h, t in\n  x == Cons h t & (fresh z in\n  x == x)) | (fresh x in\n    x == x)"

unit_prettyAddo :: Assertion
unit_prettyAddo = do
  testPretty Program.Num.addoDef "addo x y z =\n  x == O & z == y | (fresh x', z' in x == S x' & z == S z' & addo x' y z')"

unit_prettyAddoShort :: Assertion
unit_prettyAddoShort = do
  testPrettyWithWidth 20 Program.Num.addoDef "addo x y z =\n  x == O & \n  z == y | \n  (fresh x', z' in\n     x == S x' & \n     z == S z' & \n     addo x' y z')"

unit_prettyEvalo :: Assertion
unit_prettyEvalo = do
  testPretty Program.Prop.plainEvaloDef "evalo st fm u =\n  fresh x, y, v, w, z in\n    fm == Conj x y & \n    ando v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Disj x y & \n    oro v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Neg x & \n    noto v u & \n    evalo st x v | \n    fm == Var z & \n    elemo z st u"
  testPrettyWithWidth 20 Program.Prop.plainEvaloDef "evalo st fm u =\n  fresh x, y, v, w, z in\n    fm == Conj x y & \n    ando v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Disj x y & \n    oro v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Neg x & \n    noto v u & \n    evalo st x v | \n    fm == Var z & \n    elemo z st u"

unit_prettyProgram :: Assertion
unit_prettyProgram = do
  testPretty (Program Program.Prop.plainEvalo (Fresh "st" $ Fresh "fm" $ Fresh "u" $ call "evalo" [V "st", V "fm", V "u"])) "evalo st fm u =\n  fresh x, y, v, w, z in\n    fm == Conj x y & \n    ando v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Disj x y & \n    oro v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Neg x & \n    noto v u & \n    evalo st x v | \n    fm == Var z & \n    elemo z st u;\n\nando x y v =\n  x == True & \n  y == True & \n  v == True | \n  x == False & \n  y == True & \n  v == False | \n  x == True & \n  y == False & \n  v == False | \n  x == False & \n  y == False & \n  v == False;\n\noro x y v =\n  x == True & \n  y == True & \n  v == True | \n  x == False & \n  y == True & \n  v == True | \n  x == True & \n  y == False & \n  v == True | \n  x == False & \n  y == False & \n  v == False;\n\nnoto x y =\n  x == True & y == False | x == False & y == True;\n\nelemo n s v =\n  fresh h, t, n' in\n    n == O & \n    s == Cons h t & \n    v == h | \n    n == S n' & \n    s == Cons h t & \n    elemo n' t v;\n\n? fresh st, fm, u in\n    evalo st fm u"



unit_prettyProgramShort :: Assertion
unit_prettyProgramShort = do
  testPrettyWithWidth 20 (Program Program.Prop.plainEvalo (Fresh "st" $ Fresh "fm" $ Fresh "u" $ call "evalo" [V "st", V "fm", V "u"])) "evalo st fm u =\n  fresh x, y, v, w, z in\n    fm == Conj x y & \n    ando v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Disj x y & \n    oro v w u & \n    evalo st x v & \n    evalo st y w | \n    fm == Neg x & \n    noto v u & \n    evalo st x v | \n    fm == Var z & \n    elemo z st u;\n\nando x y v =\n  x == True & \n  y == True & \n  v == True | \n  x == False & \n  y == True & \n  v == False | \n  x == True & \n  y == False & \n  v == False | \n  x == False & \n  y == False & \n  v == False;\n\noro x y v =\n  x == True & \n  y == True & \n  v == True | \n  x == False & \n  y == True & \n  v == True | \n  x == True & \n  y == False & \n  v == True | \n  x == False & \n  y == False & \n  v == False;\n\nnoto x y =\n  x == True & \n  y == False | \n  x == False & \n  y == True;\n\nelemo n s v =\n  fresh h, t, n' in\n    n == O & \n    s == Cons h t & \n    v == h | \n    n == S n' & \n    s == Cons h t & \n    elemo n' t v;\n\n? fresh st, fm, u in\n    evalo st fm u"

