module Test.OfflinePD where

import           Test.Helper (manyAssert, test, test2, assertCustom, (@?=))
import           Control.Monad.State
import           BTA.NormalizeAnnotated (fresh)
import           BTA.InvokeAnnotation
import           BTA.AnnotatedDef
import           BTA.AnnotationType
import           Data.Map (fromList)
import qualified OfflinePD.AnnEnvironment as Env
import qualified OfflinePD.AnnotatedEval as E
import qualified OfflinePD.AnnUnfold as Unf
import qualified VarInterpretation as VI
import qualified FreshNames as FN
import           Syntax (Term(V, C), X, S)


unit_popingOutFreshes :: IO ()
unit_popingOutFreshes = do
    fst (evalState (E.preEval (fresh ["x", "y"] goal)) Env.empty) @?= callF x' y'
    fst (evalState (E.preEval (fresh ["x", "y"] goal)) (Env.fromDefs [fDef])) @?= callF x' y'
    evalState (E.preEval (fresh ["m", "n"] body)) Env.empty @?= (body', reverse [0..4])
  where
    x = V "x"
    y = V "y"
    m = V "m"
    n = V "n"
    h = V "h"
    t = V "t"
    cS x = C "S" [x]
    cT x = C "T" [x]
    callF x y = Invoke "f" [x, y] Unfold
    callT x = Invoke "t" [x] Unfold
    goal = callF x y
    body = fresh ["h"] (m === cS h &&& fresh ["t"] (n === cT t) &&& m === cS n) ||| fresh ["h"] (m === n &&& m === cT h &&& callT h)
    fDef = AnnotatedDef "f" ["m", "n"] body [Static, Static]
    x' = V 0
    y' = V 1
    body' = (V 0 === cS (V 2) &&& (V 1 === cT (V 3) &&& V 0 === cS (V 1))) ||| (V 0 === V 1 &&& (V 0 === cT (V 4) &&& callT (V 4)))

appendo :: (AnnG Term) X
appendo = 
    Disjunction (
        Conjunction (
            V "x" :=: C "Nil" []
        ) (
            V "y" :=: V "xy"
        ) [] 
    )  (
        Fresh "h" $ Fresh "t" $ Fresh "ty" $ 
        Conjunction (
            V "x" :=: C "Cons" [V "h", V "t"]
        ) (
            V "xy" :=: C "Cons" [V "h", V "ty"]
        ) [
            Invoke "appendo" [V "t", V "y", V "ty"] Memo
        ] 
    ) 
    [] 
    
appendoS :: (AnnG Term) S 
appendoS = 
    Disjunction (
        Conjunction (
            V 0 :=: C "Nil" []
        ) (
            V 1 :=: V 2
        ) [] 
    )  (
        Conjunction (
            V 0 :=: C "Cons" [V 3, V 4]
        ) (
            V 2 :=: C "Cons" [V 3, V 5]
        ) [
            Invoke "appendo" [V 4, V 1, V 5] Memo
        ] 
    ) 
    [] 
  
complicatedGoal :: AnnG Term S 
complicatedGoal = 
  Disjunction (
      Conjunction (
          V 0 :=: C "Nil" []
      ) (
          V 1 :=: V 2
      ) [] 
  )  (
      Conjunction (
          V 0 :=: C "Cons" [V 3, V 4]
      ) (
          V 2 :=: C "Cons" [V 3, V 5]
      ) [
        Disjunction (
              Conjunction (
                  V 4 :=: C "Nil" []
              ) (
                  V 1 :=: V 5
              ) [] 
          )  (
              Conjunction (
                  V 4 :=: C "Cons" [V 6, V 7]
              ) (
                  V 5 :=: C "Cons" [V 6, V 8]
              ) [
                  Invoke "appendo" [V 7, V 1, V 8] Memo
              ] 
          ) 
          []
      ] 
  ) 
  [] 
 
complicatedNorm :: [[AnnG Term S]]
complicatedNorm = [[V 0 :=: C "Nil" [], V 1 :=: V 2], 
                    [V 0 :=: C "Cons" [V 3, V 4], V 2 :=: C "Cons" [V 3, V 5], V 4 :=: C "Nil" [], V 1 :=: V 5], 
                    [V 0 :=: C "Cons" [V 3, V 4], V 2 :=: C "Cons" [V 3, V 5], V 4 :=: C "Cons" [V 6, V 7], V 5 :=: C "Cons" [V 6, V 8], Invoke "appendo" [V 7, V 1, V 8] Memo]]
 
defAppend :: AnnotatedDef (AnnG Term) String
defAppend = 
  AnnotatedDef "appendo" ["x", "y", "xy"] appendo [Static, Static, Dynamic]
  
env :: Env.Env
env = Env.fromDefs [defAppend]

env1 :: Env.Env 
env1 = Env.updateNames (Env.updateInterp env (foldl (\vi (key, term) -> VI.extend vi key term) VI.empty [("x", V 0), ("y", V 1), ("xy", V 2)])) (FN.FreshNames 3)
  
unit_OneStepUnfold :: IO ()
unit_OneStepUnfold = do 
  fst (evalState (E.preEval appendo) env1) @?= appendoS
  evalState (Unf.oneStepUnfold (Invoke "appendo" [V 0, V 1, V 2] Unfold)) env1 @?= appendoS
 
  
unit_normalize :: IO ()
unit_normalize = do 
  Unf.normalize appendoS @?= [[V 0 :=: C "Nil" [], V 1 :=: V 2], [V 0 :=: C "Cons" [V 3, V 4], V 2 :=: C "Cons" [V 3, V 5], Invoke "appendo" [V 4, V 1, V 5] Memo]]
  Unf.normalize complicatedGoal @?= complicatedNorm