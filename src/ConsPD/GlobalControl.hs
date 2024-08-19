{-# LANGUAGE TupleSections #-}

module ConsPD.GlobalControl where

import           Control.Monad.State
import qualified ConsPD.LocalControl as LC
import ConsPD.State

import           Data.List           (find, partition)
import qualified Data.Set            as Set 
import           Data.Tuple

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
-- import qualified Data.IntMap as Set

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
topLevel (Program defs goal) heuristic =
    let env = Env.fromDefs defs in
    let ((logicGoal, names), env') = runState (E.preEval goal) env in
    let tree = evalState (go (Descend.init [logicGoal]) Subst.empty Subst.empty) (ConsPD.State.init env') in 
    (tree, logicGoal, names) 
  where
    go :: Descend.Descend [G S] -> Subst.Subst S -> Generalizer -> State ConsPDState GlobalTree
    go d@(Descend.Descend goal ancs) subst generalizer = do 
      seen <- gets getSeen 
      sldTree <- LC.sldResolution goal subst heuristic
      modifySeen (const $ Set.insert goal seen)
      let (substs, bodies) = partition (null . snd3) $ LC.resultants sldTree 

      if null bodies 
      then return (Node d Subst.empty sldTree [])
      else do 
        nodes <- gets getSeen
        let ancs' = goal : ancs 
        let abstracted = map (abstractChild ancs') bodies -- prolly need to make it stateful (env in bodies) 
        let (toUnfold, toNotUnfold, newNodes) =
              foldl (\ (yes, no, seen) gs ->
                          let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (Embed.isVariant g) seen) gs in
                          (yes ++ brandNew, no ++ variants, Set.union (Set.fromList $ map snd4 brandNew) seen)
                    )
                    ([], [], nodes)
                    abstracted

        trees <- mapM (\(subst, g, gen, env) -> go (Descend.add g d) subst gen) toUnfold
        let forgetEnv = map (\(x, y, _) -> (x, y, Subst.empty))
        let forgetStuff = map (\(x, y, gen, _) -> (x, y, gen))
        let substLeaves = forgetEnv substs
        let leaves = forgetStuff toNotUnfold
        return (Node d generalizer sldTree (map (\(subst, g, gen) -> Leaf (Descend.init g) Subst.empty subst) (substLeaves ++ leaves) ++ trees))