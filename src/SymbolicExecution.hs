{-# LANGUAGE ScopedTypeVariables #-}

module SymbolicExecution where

import qualified CPD.LocalControl   as LC
import           Data.Foldable      (foldlM)
import           Data.List          (find, intersect, partition, (\\))
import qualified Data.Map.Strict    as M
import           Data.Maybe         (mapMaybe, fromMaybe)
import           Debug.Trace        (trace)
import           Embed
import qualified Eval               as E
import           Generalization     (generalizeGoals, generalizeSplit)
import           Prelude            hiding (or)
import           Syntax
import           Text.Printf        (printf)
import           Util.Miscellaneous (fst3, show')

data SymTree = Fail
             | Success E.Sigma
             | Or [SymTree] (G S) E.Sigma
             | Conj [SymTree] [G S] E.Sigma
             | Prune (G S) E.Sigma
             deriving (Show, Eq)

topLevel :: Int -> Program -> SymTree
topLevel depth (Program defs goal) =
    let gamma = E.updateDefsInGamma E.env0 defs in
    let (logicGoal, gamma', names) = E.preEval gamma goal in
    go logicGoal gamma' E.s0 depth
  where
    go goal _ state d | d <= 1 = Prune goal state
    go goal env@(x, y, z) state depth =
      let (unified, gamma) = oneStep goal env state in
      Or (map (\(g, s') ->
            if null g
            then
              leaf s'
            else
              Conj (map (\h -> go h gamma s' (depth - 1)) g) g s') unified)
          goal
          state

oneStep :: G S -> E.Gamma -> E.Sigma -> ([([G S], E.Sigma)], E.Gamma)
oneStep goal env state =
    let (unfolded, gamma) = LC.oneStepUnfold goal env in
    let normalized = LC.normalize unfolded in
    let unified = mapMaybe (LC.unifyStuff state) normalized in
    (unified, gamma)

leaf :: E.Sigma -> SymTree
leaf [] = Fail
leaf s  = Success s

simplify :: SymTree -> SymTree
simplify =
    go
  where
    go (Or    ch g s) = failOr ch (\x -> Or x g s)
    go (Conj  ch g s) = failConj ch (\x -> Conj x g s)
    go x = x
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

