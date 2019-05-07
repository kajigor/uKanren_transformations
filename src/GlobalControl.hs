module GlobalControl where

import qualified CPD
import Syntax
import Prelude hiding (sequence)
import Data.Maybe (isJust)
import Data.List (find, partition, inits)
import qualified Eval as E
import qualified Driving as D
import Purification
import Text.Printf
import Debug.Trace
import qualified Data.Set as Set
import qualified Tree as T
import Miscellaneous
import DotPrinter
import SldTreePrinter

type Descend = CPD.Descend

data GlobalTree = Leaf  (Descend [G S]) T.Generalizer E.Sigma
                | Node  (Descend [G S]) T.Generalizer CPD.SldTree [GlobalTree]
                | Prune (Descend [G S]) E.Sigma

sequence :: Descend a -> Set a
sequence = CPD.getAncs

branch :: GlobalTree -> Set [G S]
branch (Leaf d _ _) = sequence d
branch (Node d _ _ _) = sequence d

leaves :: GlobalTree -> Set [G S]
leaves (Leaf d _ _ ) = Set.singleton $ CPD.getCurr d
leaves (Node _ _ _ ch) =
  let sets = map leaves ch in
  foldr Set.union Set.empty sets

-- initial splitting into maximally connected suconjunctions, may be something else
part :: [G S] -> [[G S]]
part = CPD.mcs

abstract :: Descend [G S] -> [G S] -> E.Delta -> ([([G S], T.Generalizer)], E.Delta)
abstract descend goals d =
  -- trace (printf "\nAbstracting \n%s\nDescend\n%s\n" (show goals) (show descend)) $
  let qCurly = part goals in
  go (map (\x -> (x, [])) qCurly) d
   where
    go [] d@(x:_) = ([], d)
    go ((m, gen):gs) d =
      case whistle descend m of
        Nothing ->
          let (goals, delta) = go gs d in
          ((m, gen) : goals, delta)
        Just b -> let (goals, delta) = generalize m b d in
                  let goals' = if length goals == 1 && CPD.isVariant (fst $ head goals) m
                               then []
                               else goals
                  in
                  go (gs ++ goals') delta

whistle :: Descend [G S] -> [G S] -> Maybe [G S]
whistle descend m =
  let res = find (\b -> CPD.embed b m && not (CPD.isVariant b m)) (sequence descend) in
  trace (printf "Whistling\n%s\n%s" (show m) (show res)) $
  res

generalize :: [G S] -> [G S] -> E.Delta -> ([([G S], T.Generalizer)], E.Delta)
generalize m b d =
  trace "GENERALIZE" $
  let ((m1, m2), delta) = CPD.split d b m in
  let (generalized, _, gen, delta') = D.generalizeGoals d m1 b in
  (map (project gen) $ CPD.mcs generalized ++ CPD.mcs m2, delta')
    where
      project gen goals = (goals, {- filter (\(x, _) -> (V x) `elem` concatMap CPD.vars goals) -} gen)

abstractChild :: Set [G S] -> (E.Sigma, [G S], Maybe E.Gamma) -> [(E.Sigma, [G S], T.Generalizer, E.Gamma)]
abstractChild _ (_, _, Nothing) = []
abstractChild ancs (subst, g, Just env@(x, y, d)) =
  let (abstracted, delta) = abstract (CPD.Descend g ancs) g d in
  map (\(g, gen) -> (subst, g, gen, (x, y, delta))) abstracted

conjToList :: G a -> [G a]
conjToList (g :/\: h) = conjToList g ++ conjToList h
conjToList x@(Invoke _ _) = [x]
conjToList _ = error "This conjunction is not a list of calls"

topLevel :: G X -> (GlobalTree, G S, [S])
topLevel goal =
  let (goal', defs) = takeOutLets goal in
  let gamma = E.updateDefsInGamma E.env0 defs in
  let (logicGoal, gamma', names) = E.preEval' gamma goal' in
  let nodes = [[logicGoal]] in
  (go nodes (CPD.Descend [logicGoal] Set.empty) gamma' E.s0 [] [], logicGoal, names) where
    go nodes d@(CPD.Descend goal ancs) gamma subst defs generalizer =
      -- if head (trd3 gamma) <= 21
      -- then
        trace (printf "GlobalLevel:\n%s\n" $ show goal) $
        let subst = E.s0 in
        let sldTree = CPD.sldResolution goal gamma subst in
        trace (printf "\n\nSLDDDD\n%s\n\n%s\n\n\n" (show goal) $ simplyPrintTree sldTree) $
        let (substs, bodies) = partition (null . snd3) $ CPD.resultants sldTree in
        let abstracted = map (abstractChild ancs) bodies in
        let (toUnfold, toNotUnfold, newNodes) =
                foldl (\ (yes, no, seen) gs ->
                            let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (CPD.isVariant g) seen) gs in
                            (yes ++ brandNew, no ++ variants, map snd4 brandNew ++ seen)
                      )
                      ([], [], nodes)
                      abstracted
            in
        -- let leafGoals = map snd3 toUnfold in
        let (def, newDefs) = undefined in
        let ch = map (\(subst, g, gen, env) -> go newNodes (CPD.Descend g (Set.insert goal ancs)) env subst newDefs gen) toUnfold in
        let forgetEnv = map (\(x, y, _) -> (x, y, [])) in
        let forgetStuff = map (\(x, y, gen, _) -> (x,y, gen)) in
        let substLeaves = forgetEnv substs in
        let leaves = forgetStuff toNotUnfold in
        Node d generalizer sldTree (map (\(subst, g, gen) -> Leaf (CPD.Descend g Set.empty) [] subst) (substLeaves ++ leaves) ++ ch)
      -- else
      --   Prune d subst

      -- let ch = map (\((subst, g, env), ns) ->
      --                        go (ns ++ leafGoals ++ nodes) (CPD.Descend g (goal:ancs)) env subst) (zip qs (map (map snd3) $ inits qs)) in -- add qs where appropriate
      --       Node d (map (\(subst, g, _) -> Leaf (CPD.Descend g []) subst) chToNotUnfold ++ ch)
      --     else Prune d subst
