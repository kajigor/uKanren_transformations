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

[var0, var1, var2, var3, var4, var5, var6, var7, var8, var9] = map Var [0..9]

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
--                      , getVarMap = Map.fromList [ (3, FTCon "cons" [var0,var4])
--                                                 , (4, FTCon "cons" [var1,var5])
--                                                 , (5, FTCon "cons" [var2,var6])
--                                                 , (6, FTCon "nil" [])]
--                      , getNewVarMap = Map.empty } )
--   testFlattenTerm (4 :: Int)
--     (C "pair" [C "pair" [V 0, V 1], C "pair" [V 2, V 3]])
--     (4, FlattenState { getVarSource = 7
--                      , getVarMap = Map.fromList [ (4, FTCon "pair" [var5, var6])
--                          , (5, FTCon "pair" [var0, var1])
--                          , (6, FTCon "pair" [var2, var3])]
--                      , getNewVarMap = Map.empty })

testFlattenGoal :: (FreshName v, Ord v, Eq v, Show v) => v -> G v -> Goal v -> IO ()
testFlattenGoal v g exp =
  evalState (flattenGoal g) (initFlattenState v) @?= exp

unit_flattenNullaryConstructors :: IO ()
unit_flattenNullaryConstructors = do
  testFlattenGoal (0 :: Int)
    (nil :=: nil)
    (Call "success" [])

unit_flattenConstructors :: IO ()
unit_flattenConstructors = do
  testFlattenGoal (6 :: Int)
    consUnif
    (Conj (Unif (var0) (FTVar (var2))) (Unif (var1) (FTVar (var3))) [])

unit_flattenComplexConstructors :: IO ()
unit_flattenComplexConstructors = do
  testFlattenGoal (6 :: Int)
    complexUnif
    (Conj (Unif (var0) (FTCon "pair" [var6, var7]))
          (Unif (var5) (FTCon "pair" [var8, var9]))
          [Unif (var6) (FTVar var3)
          ,Unif (var7) (FTVar var4)
          ,Unif (var8) (FTVar var1)
          ,Unif (var9) (FTVar var2) ])

unit_extraVarsNeeded :: IO ()
unit_extraVarsNeeded = do
  testFlattenGoal (4 :: Int)
    (v0 :=: pair (pair v1 v2) v3)
    (Conj (Unif (var0) (FTCon "pair" [var5, var6]))
          (Unif (var4) (FTCon "pair" [var1, var2]))
          [Unif (var5) (FTVar var4)
          ,Unif (var6) (FTVar var3)])

unit_noExtraVarsNeeded :: IO ()
unit_noExtraVarsNeeded = do
  testFlattenGoal (8 :: Int)
    (pair (pair v0 v1) (pair v2 v3) :=: pair (pair v4 v5) (pair v6 v7))
    (Conj (Unif (var0) (FTVar (var4)))
          (Unif (var1) (FTVar (var5)))
          [ Unif (var2) (FTVar (var6))
          , Unif (var3) (FTVar (var7))]
    )

unit_flattenGoals :: IO ()
unit_flattenGoals = do
  let unif = v0 :=: v1
  let callF = call "f" [list vars]
  testFlattenGoal (2 :: Int)
    unif
    (Unif (var0) (FTVar (var1)))
  testFlattenGoal (4 :: Int)
    callF
    (Conj ( Call "f" [var4] )
          ( Unif (var4) (FTCon "cons" [var0,var5]) )
          [ Unif (var5) (FTCon "cons" [var1,var6])
          , Unif (var6) (FTCon "cons" [var2,var7])
          , Unif (var7) (FTCon "nil" [])])
  testFlattenGoal (4 :: Int)
    (Conjunction unif callF [])
    (Conj (Unif (var0) (FTVar (var1)))
          ( Call "f" [var4] )
          [ Unif (var4) (FTCon "cons" [var0,var5])
          , Unif (var5) (FTCon "cons" [var1,var6])
          , Unif (var6) (FTCon "cons" [var2,var7])
          , Unif (var7) (FTCon "nil" [])])
  testFlattenGoal (4 :: Int)
    (Disjunction unif callF [])
    (Disj (Unif (var0) (FTVar (var1)))
          (Conj ( Call "f" [var4] )
                ( Unif (var4) (FTCon "cons" [var0,var5]))
                [ Unif (var5) (FTCon "cons" [var1,var6])
                , Unif (var6) (FTCon "cons" [var2,var7])
                , Unif (var7) (FTCon "nil" [])])
          [])
