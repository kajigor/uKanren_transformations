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
import CpdResidualization (residualizeSldTree)

type Descend = CPD.Descend

data GlobalTree = Leaf (Descend [G S]) E.Sigma
                | Node (Descend [G S]) [GlobalTree]
                | Prune (Descend [G S]) E.Sigma

sequence :: Descend a -> Set a
--sequence d = CPD.getCurr d : CPD.getAncs d
sequence d = CPD.getAncs d

branch :: GlobalTree -> Set [G S]
branch (Leaf d _) = sequence d
branch (Node d _) = sequence d

leaves :: GlobalTree -> Set [G S]
leaves (Leaf d _ ) = Set.singleton $ CPD.getCurr d
leaves (Node _ ch) =
  let sets = map leaves ch in
  foldr Set.union Set.empty sets

-- initial splitting into maximally connected suconjunctions, may be something else
part :: [G S] -> [[G S]]
part = CPD.mcs

abstract :: Descend [G S] -> [G S] -> E.Delta -> ([[G S]], E.Delta)
abstract descend goals d =
  let qCurly = part goals in
  go qCurly d
   where
    go [] d@(x:_) = ([], d)
    go (m:gs) d =
      case whistle descend m of
        Nothing ->
          let (goals, delta) = go gs d in
          (m : goals, delta)
        Just b -> let (goals, delta) = generalize m b d
                  in go (gs ++ goals) delta


-- abstract :: Descend [G S] -> [G S] -> E.Delta -> [[G S]]
-- abstract descend goals d =
--   let qCurly = part goals in
--   let result = go qCurly
--   in trace (printf "Abstracting\n%s\nIn the context of\n%s\nWith result\n%s\n" (show goals) (show (sequence descend)) (show result) )  $ result
--    where
--     go [] = []
--     go (m:gs) = maybe (m : go gs) (\b -> go (gs ++ generalize m b d)) (whistle descend m)

whistle :: Descend [G S] -> [G S] -> Maybe [G S]
whistle descend m =
  find (\b -> CPD.embed b m && not (CPD.isVariant b m)) (sequence descend)

  --find (\b -> CPD.embed b m && b /= m) (sequence descend)

generalize :: [G S] -> [G S] -> E.Delta -> ([[G S]], E.Delta)
generalize m b d =
  let ((m1, m2), delta) = CPD.split d b m in
  let (generalized, _, _, delta') = D.generalizeGoals d m1 b in
  (CPD.mcs generalized ++ CPD.mcs m2, delta')



-- generalize :: [G S] -> [G S] -> E.Delta -> [[G S]]
-- generalize m b d =
--   let (m1, m2) = CPD.split d b m in -- TODO keep track of DELTA!!! otherwise you'll end up colliding var names
--   CPD.mcs ( (\(x,_,_,_) -> x) $ D.generalizeGoals d m1 b) ++ CPD.mcs m2

-- abstractChildren :: [(E.Sigma, [G S], Maybe E.Gamma)] -> [[G S]] -> [(E.Sigma, [G S], E.Gamma)]
-- abstractChildren [] _ = []
-- abstractChildren ((subst, g, Just env@(x, y, d)) : gs) ancs =
--   let (abstracted, delta) = abstract (CPD.Descend g ancs) g d in
--   let mapped = map (\g -> (subst, g, (x, y, delta))) abstracted in
--   let result = mapped ++ abstractChildren gs ancs in
--   -- trace (printf "abstractChildren: %s" (show $ map second result)) $
--   result

abstractChild :: Set [G S] -> (E.Sigma, [G S], Maybe E.Gamma) -> [(E.Sigma, [G S], E.Gamma)]
abstractChild _ (_, _, Nothing) = []
abstractChild ancs (subst, g, Just env@(x, y, d)) =
  let (abstracted, delta) = abstract (CPD.Descend g ancs) g d in
  map (\g -> (subst, g, (x, y, delta))) abstracted

second (_, x, _) = x

conjToList :: G a -> [G a]
conjToList (g :/\: h) = conjToList g ++ conjToList h
conjToList x@(Invoke _ _) = [x]
conjToList _ = error "This conjunction is not a list of calls"

topLevel :: G X -> GlobalTree
topLevel goal =
  let (goal', defs) = takeOutLets goal in
  let gamma = E.updateDefsInGamma E.env0 defs in
  let (logicGoal, gamma', names) = E.preEval' gamma goal' in
  let nodes = [[logicGoal]] in
  go nodes (CPD.Descend [logicGoal] Set.empty) gamma' E.s0 [] where
    go nodes d@(CPD.Descend goal ancs) gamma subst defs =
      -- if any (\g -> any (\g -> case g of Invoke "add" [_, C "S" [C "S" [C "S" [C "S" [C "S" [_]]]]], _] -> True ; _ -> False) g) $ Set.toList ancs
      -- then
        -- trace "\n\nglobal level\n\n" $
        let subst = E.s0 in
        let sldTree = CPD.sldResolution goal gamma subst in
        let (substs, bodies) = partition (null . second) $ CPD.resultants sldTree in
        let (def, newDefs) = residualizeSldTree (concatMap conjToList goal) sldTree defs in
        trace (printf "\nResidualized\n%s" (show def)) $
        let abstracted = map (abstractChild ancs) bodies in
        let (toUnfold, toNotUnfold, newNodes) =
                foldl (\ (yes, no, seen) gs ->
                            let (variants, brandNew) = partition (\(_, g, _) -> null g || any (CPD.isVariant g) seen) gs in
                            (yes ++ brandNew, no ++ variants, map second brandNew ++ seen)
                      )
                      ([], [], nodes)
                      abstracted
            in
        -- let leafGoals = map second toUnfold in
        let ch = map (\(subst, g, env) -> go newNodes (CPD.Descend g (Set.insert goal ancs)) env subst newDefs) toUnfold in
        let forgetEnv = map (\(x, y, _) -> (x, y)) in
        let substLeaves = forgetEnv substs in
        let leaves = forgetEnv toNotUnfold in
        Node d (map (\(subst, g) -> Leaf (CPD.Descend g Set.empty) subst) (substLeaves ++ leaves) ++ ch)
      -- else
      --   Prune d subst
      -- let ch = map (\((subst, g, env), ns) ->
      --                        go (ns ++ leafGoals ++ nodes) (CPD.Descend g (goal:ancs)) env subst) (zip qs (map (map second) $ inits qs)) in -- add qs where appropriate
      --       Node d (map (\(subst, g, _) -> Leaf (CPD.Descend g []) subst) chToNotUnfold ++ ch)
      --     else Prune d subst


  -- go nodes (CPD.Descend [logicGoal] []) gamma' E.s0 where
  --   go nodes d@(CPD.Descend goal ancs) gamma subst =
  --     (case goal of
  --       [Invoke "maxo1" (V 15 : _)] ->
  --         trace (printf "\nGlobal:\ngoal:  %s\nnodes: %s\n" (show goal) (show nodes))
  --       _ -> id) $
  --     if length nodes < 10
  --     then
  --       --trace "global level" $
  --       let sldtree = CPD.sldResolutionStep (map (\x -> CPD.Descend x []) goal) gamma subst [] in
  --       let (chToUnfold, chToNotUnfold) =
  --             (\(x, y) -> (reverse x, reverse y)) $
  --             foldl (\(yes, no) r@(_, g, _) ->
  --                     let check g g1 = (case g of
  --                                        [Invoke "maxo1" (V 15 : _)] ->
  --                                          trace (printf "\ng:  %s\ng1: %s\n" (show g) (show g1))  -- (printf "\nGlobal:\ngoal:  %s\nnodes: %s\nIs variant: %s\n" (show goal) (show nodes) (show $ any (CPD.isVariant g) (nodes ++ map second yes)))
  --                                        _ -> id) $
  --                                      (CPD.isVariant g g1) in
  --                     if null g || any (check g) (nodes ++ map second yes) -- (CPD.isVariant g) (nodes ++ map second yes)
  --                     then (yes, r : no)
  --                     else (r : yes, no))
  --                   ([], [])
  --                   (CPD.resultants sldtree) in
  --       -- let (chToUnfold, chToNotUnfold) = partition (\(_, g, _) -> not (null g || any (CPD.isVariant g) nodes)) (CPD.resultants sldtree) in
  --       --let qs = concatMap (\(subst, g, Just env@(_,_, d)) -> map (\g -> (subst, g, env)) $ abstract (CPD.Descend g ancs) g d) chToUnfold in
  --       (case goal of
  --         [Invoke "maxo1" (V 15 : _)] ->
  --           trace (printf "\nTo unfold:  %s\nNot unfold: %s\n" (show (map second chToUnfold)) (show (map second chToNotUnfold)))
  --         _ -> id) $
  --       let qs = abstractChildren chToUnfold ancs in
  --       -- let leafGoals = map second qs in
  --       let leafGoals = filter (\q -> trace (printf "checking %s\n" (show q)) $ any (CPD.isVariant q) (nodes ++ map second chToNotUnfold) ) $ map second qs in
  --       (case goal of
  --                 [Invoke "maxo1" (V 15 : _)] -> trace (printf "\nAbstracted:  %s\n" (show leafGoals))
  --                 _ -> id) $
  --       let ch = map (\((subst, g, env), ns) ->
  --                        --trace (printf "\nRecurring in go with\n\nns: %s\n\nleafGoals: %s\n\nNodes: %s\n" (show ns) (show leafGoals) (show nodes)) $
  --                        go (ns ++ leafGoals ++ nodes) (CPD.Descend g (goal:ancs)) env subst) (zip qs (map (map second) $ inits qs)) in -- add qs where appropriate
  --       Node d (map (\(subst, g, _) -> Leaf (CPD.Descend g []) subst) chToNotUnfold ++ ch)
  --     else Prune d subst
