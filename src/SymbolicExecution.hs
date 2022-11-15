{-# LANGUAGE ScopedTypeVariables #-}

module SymbolicExecution where

import qualified Eval               as E
import           Prelude            hiding (or)
import qualified Subst
import           Syntax
import Program
import           Unfold             (oneStep)
import qualified Environment as Env
import Control.Monad.State

data SymTree = Fail
             | Success Subst.Subst
             | Disj [SymTree] [G S] Subst.Subst
             | Conj [SymTree] [G S] Subst.Subst
             | Prune [G S] Subst.Subst
             deriving (Show, Eq)

topLevel :: Int -> Program G X -> SymTree
topLevel depth (Program defs goal) =
    let env = Env.fromDefs defs in
    let ((logicGoal, _), env') = runState (E.preEval goal) env in
    go logicGoal [] env' Subst.empty depth
  where
    go goal ctx _ subst d | d <= 1 = Prune (goal : ctx) subst
    go goal ctx env subst depth =
      let (unified, env') = runState (oneStep goal subst) env in
      Disj (map (\(g, s') ->
                  if null g
                  then
                    leaf s'
                  else
                    Conj [go (head g) (tail g) env' s' (depth - 1)] g s'
                )
                ((\(g, s) -> (g++ctx, s)) <$> unified))
            (goal : ctx)
            subst

leaf :: Subst.Subst -> SymTree
leaf x = if Subst.null x then Fail else Success x

simplify :: SymTree -> SymTree
simplify =
    go
  where
    go (Disj ch g s) = failOr   ch (\x -> Disj x g s)
    go (Conj ch g s) = failConj ch (\x -> Conj x g s)
    go x             = x
    failOr ch f =
      let simplified = filter (/= Fail) $ map go ch in
      if null simplified
      then Fail
      else f simplified
    failConj ch f =
      let simplified = map go ch in
      if Fail `elem` simplified
      then Fail
      else f simplified

