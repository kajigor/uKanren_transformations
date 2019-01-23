module GlobalControl where

import qualified CPD as CPD
import Syntax
import Prelude hiding (sequence)
import Data.Maybe (isJust)
import Data.List (find, partition)
import qualified Eval as E
import qualified Driving as D
import Purification
import Text.Printf
import Debug.Trace

type Descend = CPD.Descend

data GlobalTree = Leaf (Descend [G S]) E.Sigma
                | Node (Descend [G S]) [GlobalTree]

sequence :: Descend a -> [a]
sequence d = CPD.getCurr d : CPD.getAncs d

branch :: GlobalTree -> [[G S]]
branch (Leaf d _) = sequence d
branch (Node d _) = sequence d

leaves :: GlobalTree -> [[G S]]
leaves (Leaf d _ ) = [CPD.getCurr d]
leaves (Node _ ch) = concatMap leaves ch

-- initial splitting into maximally connected suconjunctions, may be something else
part :: [G S] -> [[G S]]
part = CPD.mcs

abstract :: Descend [G S] -> [G S] -> E.Delta -> [[G S]]
abstract descend gs d =
  let qCurly = part gs in
  go qCurly where
    go [] = []
    go (m:gs) = maybe (m : go gs) (\b -> go (gs ++ generalize descend m b d)) (whistle descend m)

whistle :: Descend [G S] -> [G S] -> Maybe [G S]
whistle descend m =
  find (\b -> CPD.embed b m && b /= m) (sequence descend)

generalize :: Descend [G S] -> [G S] -> [G S] -> E.Delta -> [[G S]]
generalize leaf m b d =
  let (m1, m2) = CPD.split d b m in
  CPD.mcs ( (\(x,_,_,_) -> x) $ D.generalizeGoals d m1 b) ++ CPD.mcs m2

second (_, x, _) = x

topLevel :: G X -> GlobalTree
topLevel goal =
  let (goal', _, defs) = justTakeOutLets (goal, []) in
  let gamma = E.updateDefsInGamma E.env0 defs in
  let (logicGoal, gamma', names) = E.preEval' gamma goal' in
  let nodes = [[logicGoal]] in
  go nodes (CPD.Descend [logicGoal] []) gamma' E.s0 where
    go nodes d@(CPD.Descend goal ancs) gamma subst =
      let sldtree = CPD.sldResolutionStep (map (\x -> CPD.Descend x []) goal) gamma subst [] in
      let (chToUnfold, chToNotUnfold) =
            (\(x, y) -> (reverse x, reverse y)) $
            foldl (\(yes, no) r@(_, g, _) -> if null g || any (CPD.isVariant g) (nodes ++ map second yes)
                                             then (yes, r : no)
                                             else (r : yes, no))
                  ([], [])
                  (CPD.resultants sldtree) in
      -- let (chToUnfold, chToNotUnfold) = partition (\(_, g, _) -> not (null g || any (CPD.isVariant g) nodes)) (CPD.resultants sldtree) in
      trace (printf "ChildrenToUnfold: %s\nToNotUnfold:%s\nNodes: %s\n" (show $ map second chToUnfold) (show $ map second chToNotUnfold) (show nodes)) $
      let qs = concatMap (\(subst, g, Just env@(_,_, d)) -> map (\g -> (subst, g, env)) $ abstract (CPD.Descend g ancs) g d) chToUnfold in
      let leafGoals = map second qs in
      let ch = map (\(subst, g, env) -> go (leafGoals ++ nodes) (CPD.Descend g (goal:ancs)) env subst) qs in
      Node d (map (\(subst, g, _) -> Leaf (CPD.Descend g []) subst) chToNotUnfold ++ ch)

  -- let sldtree = sldResolutionStep [Descend logicGoal []] gamma' E.s0 [] in
  -- let ch = resultants sldTree in
  -- let chToUnfold = filter (\(_, g, _) -> not (null g || any (isVariant g) nodes)) ch in
