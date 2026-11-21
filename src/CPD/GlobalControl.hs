{-# LANGUAGE TupleSections #-}

module CPD.GlobalControl where

import           Control.Monad.State 
import qualified CPD.LocalControl    as LC
import           CPD.State 
import           Data.List           (find, partition)
import qualified Data.Set            as Set 
import qualified Data.Map.Strict     as Map
import           Descend
import           Embed
import qualified Environment         as Env
import qualified Eval                as E
import qualified FreshNames          as FN
import           Generalization      (Generalizer)
import           Prelude             hiding (sequence)
import           Program
import qualified Subst
import           Syntax
import           Util.Miscellaneous
import Debug.Trace

data GlobalTree = Leaf  (Descend [G S]) Generalizer (Subst.Subst S)
                | Node  (Descend [G S]) Generalizer LC.SldTree [GlobalTree]
                | Prune (Descend [G S]) (Subst.Subst S)
                deriving (Show)

sequence :: Descend a -> [a]
sequence = getAncs

getNodes (Leaf _ _ _) = []
getNodes (Node _ _ (LC.Leaf _ _ _) _) = [] -- This happens because of the instance check
getNodes (Node d _ sld ch) = (getCurr d, sld) : concatMap getNodes ch

-- initial splitting into maximally connected suconjunctions, may be something else
part :: [G S] -> [[G S]]
part = LC.mcs

abstract :: Descend [G S] -> [G S] -> FN.FreshNames -> ([([G S], Generalizer)], FN.FreshNames)
abstract descend goals d =
  let qCurly = part goals in
  let result = go (map (, Subst.empty) qCurly) d in
  result
   where
    go [] d = ([], d)
    go ((m, gen):gs) d =
      case whistle descend m of
        Nothing ->
          let (goals, delta) = go gs d in
          ((m, gen) : goals, delta)
        Just b ->
          let (goals, delta) = generalize m b d in
          let (blah, halb) = go gs delta in
          (goals ++ blah, halb)

whistle :: Descend [G S] -> [G S] -> Maybe [G S]
whistle descend m =
  find (\b -> Embed.embed b m && not (Embed.isVariant b m)) (sequence descend)

generalize :: [G S] -> [G S] -> FN.FreshNames -> ([([G S], Generalizer)], FN.FreshNames)
generalize m b d =
    let ((m1, m2), genOrig, delta) = LC.split d b m in 
    let genTrue = genOrig in
    (map (project genTrue) (LC.mcs m1) ++ map (project Subst.empty) (LC.mcs m2), delta)
  where
    project gen goals = (goals, gen)

abstractChild :: [[G S]] -> (Subst.Subst S, [G S], Maybe Env.Env) -> [(Subst.Subst S, [G S], Generalizer, Env.Env)]
abstractChild _ (_, _, Nothing) = []
abstractChild ancs (subst, g, Just env) =
  let (abstracted, d') = abstract (Descend.Descend g ancs) g (Env.getFreshNames env) in
  map (\(g, gen) -> (subst, g, gen, Env.updateNames env d')) abstracted

topLevel :: Program G X -> LC.Heuristic -> (GlobalTree, G S, [S])
topLevel = topLevelWithBudget Nothing Nothing

-- | Generalize every atom of a conjunction to its most general form
--   p(X1, .., Xn).  There are finitely many such goals (one per relation), so
--   once the budget is exhausted global control terminates quickly: the
--   residual program then calls (copies of) the original relations.
mostGeneral :: (Subst.Subst S, [G S], Maybe Env.Env) -> [(Subst.Subst S, [G S], Generalizer, Env.Env)]
mostGeneral (_, _, Nothing) = []
mostGeneral (subst, gs, Just env) =
  let go fn [] = ([], fn)
      go fn (Invoke f ts : rest) =
        let (vs, fn') = FN.getNames (length ts) fn
            (more, fn'') = go fn' rest
        in ([Invoke f (map V vs)] : more, fn'')
      go fn (_ : rest) = go fn rest
      (pieces, fn1) = go (Env.getFreshNames env) gs
  in [ (subst, p, Subst.empty, Env.updateNames env fn1) | p <- pieces ]

-- | Global control with an optional budget on the number of global nodes.
--   Budget exhausted: no further specialization, every new leaf is
--   generalized to most general atoms ("anytime" specialization).
topLevelWithBudget :: Maybe Int -> Maybe Int -> Program G X -> LC.Heuristic -> (GlobalTree, G S, [S])
topLevelWithBudget budget localBudget prog heuristic =
    let (tree, logicGoal, names, _) = topLevelAbstr budget localBudget prog heuristic in
    (tree, logicGoal, names)

-- | Like 'topLevelWithBudget', also returning how resultants were abstracted.
topLevelAbstr :: Maybe Int -> Maybe Int -> Program G X -> LC.Heuristic
              -> (GlobalTree, G S, [S], Map.Map ([G S], [G S]) [([G S], Generalizer)])
topLevelAbstr budget localBudget (Program defs goal) heuristic =
    let env = Env.fromDefs defs in
    let ((logicGoal, names), env') = runState (E.preEval goal) env in
    let (tree, st) = runState (go (Descend.init [logicGoal]) Subst.empty Subst.empty) (CPD.State.initWithBudget localBudget env') in
    (tree, logicGoal, names, getAbstr st)
  where
    go :: Descend.Descend [G S] -> Subst.Subst S -> Generalizer -> State CPDState GlobalTree
    go d@(Descend.Descend goal ancs) subst generalizer = do 
      seen <- gets getSeen 
      sldTree <- LC.sldResolution goal subst heuristic
      modifySeen (const $ Set.insert goal seen)
      let (substs, bodies) = partition (null . snd3) $ LC.resultants sldTree 
      let substLeaves = map leafOutOfBody $ forgetEnv substs

      if null bodies 
      -- then return (Node d generalizer sldTree [])
      then return (Node d generalizer sldTree substLeaves)
      else do 
        nodes <- gets getSeen
        let ancs' = goal : ancs 
        let exhausted = maybe False (Set.size nodes >=) budget
        let abstracted = if exhausted
                         then map mostGeneral bodies
                         else map (abstractChild ancs') bodies -- prolly need to make it stateful (env in bodies) 
        -- record how every resultant was split, for the residualizer
        modify (\st -> st { getAbstr = foldl (\m ((_, bgs, _), pcs) ->
                                Map.insert (goal, bgs) [ (g, gen) | (_, g, gen, _) <- pcs ] m)
                              (getAbstr st) (zip bodies abstracted) })
        let (toUnfold, toNotUnfold, newNodes) =
              foldl (\ (yes, no, seen) gs ->
                          let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (Embed.isVariant g) seen) gs in
                          (yes ++ brandNew, no ++ variants, Set.union (Set.fromList $ map snd4 brandNew) seen)
                    )
                    ([], [], nodes)
                    abstracted

        trees <- mapM (\(subst, g, gen, env) -> do modifyEnv (const env); go (Descend.add g d) subst gen) toUnfold

        let leaves = forgetStuff toNotUnfold
        return (Node d generalizer sldTree (substLeaves ++ map (\(subst, g, gen) -> Leaf (Descend.init g) Subst.empty subst) leaves ++ trees))

    leafOutOfBody (subst, g, gen) = Leaf (Descend.init g) Subst.empty subst 

    forgetEnv = map (\(x, y, _) -> (x, y, Subst.empty))
    forgetStuff = map (\(x, y, gen, _) -> (x, y, gen))