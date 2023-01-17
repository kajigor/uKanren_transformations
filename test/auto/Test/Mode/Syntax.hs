module Test.Mode.Syntax where

import           Control.Monad.State
import           FreshNames
import           Mode.Syntax
import           Mode.Term
import           Syntax
import           Test.Helper         ((@?=))

v0, v1, v2 :: Term Int
vars :: [Term Int]
vars@[v0, v1, v2] = map V [0, 1, 2]

cons :: Term v -> Term v -> Term v
cons x y = C "cons" [x, y]

list :: [Term v] -> Term v
list = foldr cons (C "nil" [])

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
