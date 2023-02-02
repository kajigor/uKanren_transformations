module Test.Parser.SimplePretty where

import           Control.Monad       (zipWithM_)
import           Parser.SimplePretty
import           Syntax
import           Test.Helper

x, y, z, m, n, h, t :: Term X
[x, y, z, m, n, h, t] = map V ["x", "y", "z", "m", "n", "h", "t"]

v0, v1, v2, v3, v13, v42 :: Term S
[v0, v1, v2, v3, v13, v42] = map V [0, 1, 2, 3, 13, 42]

testPretty :: SimplePretty a => a -> String -> Assertion
testPretty = test prettyString

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
  testPretty (Fresh "x" (x :=: x)) "fresh x in x == x"
  testPretty (Fresh "x" $ Fresh "y" $ Fresh "z" (x :=: y)) "fresh x, y, z in x == y"
