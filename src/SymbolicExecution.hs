{-# LANGUAGE ScopedTypeVariables #-}

module SymbolicExecution where

import qualified Eval               as E
import           Prelude            hiding (or)
import qualified Subst
import           Syntax
import           Unfold             (oneStep)

data SymTree = Fail
             | Success Subst.Subst
             | Disj [SymTree] [G S] Subst.Subst
             | Conj [SymTree] [G S] Subst.Subst
             | Prune [G S] Subst.Subst
             deriving (Show, Eq)

topLevel :: Int -> Program -> SymTree
topLevel depth (Program defs goal) =
    let gamma = E.gammaFromDefs defs in
    let (logicGoal, gamma', _) = E.preEval gamma goal in
    go logicGoal [] gamma' Subst.empty depth
  where
    go goal ctx _ state d | d <= 1 = Prune (goal : ctx) state
    go goal ctx env@(x, y, z) state depth =
      let (unified, gamma) = oneStep goal env state in
      Disj (map (\(g, s') ->
                  if null g
                  then
                    leaf s'
                  else
                    Conj [go (head g) (tail g) gamma s' (depth - 1)] g s'
                )
                ((\(g, s) -> (g++ctx, s)) <$> unified))
            (goal : ctx)
            state

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

