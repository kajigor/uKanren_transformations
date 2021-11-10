module Test.VarNormalization where

import Transformer.VarNormalization
import           Test.Helper  (test)
import Syntax
import Program.List (appendo)

x, y, z, q0, q1, q2, q3, x0, x1, x2, x3 :: Term [Char]
x = V "x"
y = V "y"
z = V "z"
q0 = V "q.0"
q1 = V "q.1"
q2 = V "q.2"
q3 = V "q.3"
x0 = V "x.0"
x1 = V "x.1"
x2 = V "x.2"
x3 = V "x.3"

a, b, c :: [Term v] -> Term v
a = C "a"
b = C "b"
c = C "c"

unit_append = do
    runTest (Program appendo (fresh ["xs", "ys", "zs"] $ Invoke "appendo" [V "xs", V "ys", V "zs"] ))
            (Program [Def "appendo" ["x.0", "x.1", "x.2"]
                           (Disjunction (Conjunction (x0 === C "Nil" [])
                                                     (x2 === x1)
                                                     [])
                                        (fresh ["q.0", "q.1", "q.2"] (Conjunction (x0 === C "Cons" [q0, q1])
                                                                                  (Conjunction (x2 === C "Cons" [q0, q2])
                                                                                               (call "appendo" [q1, x1, q2])
                                                                                               [])
                                                                                  []))
                                        [])]
                           (fresh ["q.0", "q.1", "q.2"] (call "appendo" [q0, q1, q2])))
  where
    runTest =
      test uniqueVarRename

unit_unique = do
    runTest (Program [Def "f" ["x", "y"] (x === y)] (fresh ["q0"] $ Invoke "f" [V "q0", V "q0"] ))
            (Program [Def "f" ["x.0", "x.1"] (x0 === x1)] (fresh ["q.0"] $ Invoke "f" [q0, q0] ))
  where
    runTest =
      test uniqueVarRename


unit_uniqueDisjunction = do
    runTest (Fresh "x" $ Fresh "y" $ Fresh "z" (Disjunction (x === y) (y === z) [x === z]))
            (Fresh "q.0" $ Fresh "q.1" $ Fresh "q.2" $ Disjunction (q0 === q1) (q1 === q2) [q0 === q2])
    runTest (Disjunction (Fresh "x" $ Fresh "y" (x === y))
                         (Fresh "x" $ Fresh "z" (x === a [z, x, z]))
                         [])
            (Disjunction (Fresh "q.0" $ Fresh "q.1" (q0 === q1))
                         (Fresh "q.2" $ Fresh "q.3" (q2 === a [q3, q2, q3]))
                         [])
  where
    runTest =
      test (\g -> fst $ uniqueVarGoal g [])

unit_uniqueConjunction = do
    runTest (Fresh "x" $ Fresh "y" $ Fresh "z" (Conjunction (x === y) (y === z) [x === z]))
            (Fresh "q.0" $ Fresh "q.1" $ Fresh "q.2" $ Conjunction (q0 === q1) (q1 === q2) [q0 === q2])
  where
    runTest =
      test (\g -> fst $ uniqueVarGoal g [])

unit_uniqueUnification = do
    runTest (Fresh "x" $ Fresh "y" (x === y)) (Fresh "q.0" $ Fresh "q.1" $ q0 === q1)
    runTest (Fresh "x" $ Fresh "x" (x === x)) (Fresh "q.0" $ Fresh "q.1" $ q1 === q1)
    runTest (Fresh "x" $ Fresh "y" $ Fresh "x" (x === y)) (Fresh "q.0" $ Fresh "q.1" $ Fresh "q.2" $ q2 === q1)
  where
    runTest =
      test (\g -> fst $ uniqueVarGoal g [])