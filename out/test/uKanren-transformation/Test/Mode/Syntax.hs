module Test.Mode.Syntax where

import           Control.Monad.State
import           FreshNames
import           Mode.Syntax
import           Mode.Term
import           Syntax
import           Test.Helper         ((@?=))

v0, v1, v2, v3, v4, v5, v6, v7 :: Term Int
vars :: [Term Int]
vars@[v0, v1, v2] = map V [0, 1, 2]

[v3, v4, v5, v6, v7] = map V [3, 4, 5, 6, 7]

nil :: Term a
nil = C "nil" []

cons :: Term v -> Term v -> Term v
cons x y = C "cons" [x, y]

pair :: Term v -> Term v -> Term v
pair x y = C "pair" [x, y]

list :: [Term v] -> Term v
list = foldr cons (C "nil" [])

consUnif :: G Int
consUnif = cons v0 v1 :=: cons v2 v3

complexUnif :: G Int
complexUnif = pair v0 (pair v1 v2) :=: pair (pair v3 v4) v5

-- testFlattenTerm v t exp =
--   runState (flattenTerm t) (flattenGoal v) @?= exp

-- unit_flattenTerm = do
--   testFlattenTerm 3
--     (list [v0, v1, v2])
--     (3, FlattenState { getVarSource = 7
--                      , getVarMap = Map.fromList [ (3, FTCon "cons" [Var 0,Var 4])
--                                                 , (4, FTCon "cons" [Var 1,Var 5])
--                                                 , (5, FTCon "cons" [Var 2,Var 6])
--                                                 , (6, FTCon "nil" [])]
--                      , getNewVarMap = Map.empty } )
--   testFlattenTerm (4 :: Int)
--     (C "pair" [C "pair" [V 0, V 1], C "pair" [V 2, V 3]])
--     (4, FlattenState { getVarSource = 7
--                      , getVarMap = Map.fromList [ (4, FTCon "pair" [Var 5, Var 6])
--                          , (5, FTCon "pair" [Var 0, Var 1])
--                          , (6, FTCon "pair" [Var 2, Var 3])]
--                      , getNewVarMap = Map.empty })

testFlattenGoal :: (FreshName v, Ord v, Eq v, Show v) => v -> G v -> Goal v -> IO ()
testFlattenGoal v g exp =
  evalState (flattenGoal g) (initFlattenState v) @?= exp

unit_flattenNullaryConstructors :: IO ()
unit_flattenNullaryConstructors = do
  testFlattenGoal (0 :: Int)
    (nil :=: nil)
    (Unif (Var 0) (FTVar $ Var 0))

unit_flattenConstructors :: IO ()
unit_flattenConstructors = do
  testFlattenGoal (6 :: Int)
    consUnif
    (Conj (Unif (Var 0) (FTVar (Var 2))) (Unif (Var 1) (FTVar (Var 3))) [])

unit_flattenComplexConstructors :: IO ()
unit_flattenComplexConstructors = do
  testFlattenGoal (6 :: Int)
    complexUnif
    (Conj (Unif (Var 0) (FTCon "pair" [Var 3, Var 4])) (Unif (Var 5) (FTCon "pair" [Var 1, Var 2])) [])

unit_extraVarsNeeded :: IO ()
unit_extraVarsNeeded = do
  testFlattenGoal (4 :: Int)
    (v0 :=: pair (pair v1 v2) v3)
    (Conj (Unif (Var 0) (FTCon "pair" [Var 4, Var 3])) (Unif (Var 4) (FTCon "pair" [Var 1, Var 2])) [])

unit_noExtraVarsNeeded :: IO ()
unit_noExtraVarsNeeded = do
  testFlattenGoal (8 :: Int)
    (pair (pair v0 v1) (pair v2 v3) :=: pair (pair v4 v5) (pair v6 v7))
    (Conj (Unif (Var 0) (FTVar (Var 4)))
          (Unif (Var 1) (FTVar (Var 5)))
          [ Unif (Var 2) (FTVar (Var 6))
          , Unif (Var 3) (FTVar (Var 7))]
    )

unit_flattenGoals :: IO ()
unit_flattenGoals = do
  let unif = v0 :=: v1
  let callF = call "f" [list vars]
  testFlattenGoal (2 :: Int)
    unif
    (Unif (Var 0) (FTVar (Var 1)))
  testFlattenGoal (4 :: Int)
    callF
    (Conj ( Call "f" [Var 4] )
          ( Unif (Var 4) (FTCon "cons" [Var 0,Var 5]) )
          [ Unif (Var 5) (FTCon "cons" [Var 1,Var 6])
          , Unif (Var 6) (FTCon "cons" [Var 2,Var 7])
          , Unif (Var 7) (FTCon "nil" [])])
  testFlattenGoal (4 :: Int)
    (Conjunction unif callF [])
    (Conj (Unif (Var 0) (FTVar (Var 1)))
          ( Call "f" [Var 4] )
          [ Unif (Var 4) (FTCon "cons" [Var 0,Var 5])
          , Unif (Var 5) (FTCon "cons" [Var 1,Var 6])
          , Unif (Var 6) (FTCon "cons" [Var 2,Var 7])
          , Unif (Var 7) (FTCon "nil" [])])
  testFlattenGoal (4 :: Int)
    (Disjunction unif callF [])
    (Disj (Unif (Var 0) (FTVar (Var 1)))
          (Conj ( Call "f" [Var 4] )
                ( Unif (Var 4) (FTCon "cons" [Var 0,Var 5]))
                [ Unif (Var 5) (FTCon "cons" [Var 1,Var 6])
                , Unif (Var 6) (FTCon "cons" [Var 2,Var 7])
                , Unif (Var 7) (FTCon "nil" [])])
          [])
