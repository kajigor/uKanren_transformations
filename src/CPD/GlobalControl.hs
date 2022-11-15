{-# LANGUAGE TupleSections #-}

module CPD.GlobalControl where

import qualified CPD.LocalControl   as LC
import           Data.List          (find, partition)
import           Data.Tuple
import           Descend
import           Debug.Trace        (trace)
import           Embed
import qualified Eval               as E
import qualified FreshNames         as FN
import           Generalization     (Generalizer)
import           Prelude            hiding (sequence)
import qualified Subst
import           Syntax
import           Def
import           Program
import           Text.Printf        ( printf )
import           Util.Miscellaneous
import qualified Environment as Env
import Control.Monad.State (runState)

data GlobalTree = Leaf  (Descend [G S]) Generalizer Subst.Subst
                | Node  (Descend [G S]) Generalizer LC.SldTree [GlobalTree]
                | Prune (Descend [G S]) Subst.Subst

sequence :: Descend a -> [a]
sequence = getAncs

getNodes (Leaf _ _ _) = []
getNodes (Node _ _ (LC.Leaf _ _ _) _) = [] -- This happens because of the instance check
getNodes (Node d _ sld ch) = (getCurr d, sld) : concatMap getNodes ch

{- branch :: GlobalTree -> Set [G S]
branch (Leaf d _ _) = sequence d
branch (Node d _ _ _) = sequence d

leaves :: GlobalTree -> Set [G S]
leaves (Leaf d _ _ ) = Set.singleton $ LC.getCurr d
leaves (Node _ _ _ ch) =
  let sets = map leaves ch in
  foldr Set.union Set.empty sets -}

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
          -- trace ("generalize done " ++ show' goals) $
          let (blah, halb) = go gs delta in
          (goals ++ blah, halb)

whistle :: Descend [G S] -> [G S] -> Maybe [G S]
whistle descend m =
  find (\b -> Embed.embed b m && not (Embed.isVariant b m)) (sequence descend)

generalize :: [G S] -> [G S] -> FN.FreshNames -> ([([G S], Generalizer)], FN.FreshNames)
generalize m b d =
  let ((m1, m2), genOrig, delta) = LC.split d b m in -- TODO PCHM
  let genTrue = genOrig in
  --let (generalized, _, gen, delta') = D.generalizeGoals d m1 b in
  -- trace (printf "\nAfter split\n%s\n" (show' $ LC.mcs m1)) $
  (map (project genTrue) (LC.mcs m1) ++ map (project Subst.empty) (LC.mcs m2), delta)
  -- (map (project genTrue) [m1] ++ map (project []) (LC.mcs m2), delta)
    where
      project gen goals = (goals, {- filter (\(x, _) -> (V x) `elem` concatMap LC.vars goals) -} gen)

abstractChild :: [[G S]] -> (Subst.Subst, [G S], Maybe Env.Env) -> [(Subst.Subst, [G S], Generalizer, Env.Env)]
abstractChild _ (_, _, Nothing) = []
abstractChild ancs (subst, g, Just env) =
  let (abstracted, d') = abstract (Descend.Descend g ancs) g (Env.getFreshNames env) in
  map (\(g, gen) -> (subst, g, gen, Env.updateNames env d')) abstracted

topLevel :: Program G X -> LC.Heuristic -> (GlobalTree, G S, [S])
topLevel (Program defs goal) heuristic =
  let env = Env.fromDefs defs in
  let ((logicGoal, names), env') = runState (E.preEval goal) env in
  -- trace (printf "\nGoal:\n%s\nNames:\n%s\n" (show goal) (show names)) $
  let nodes = [[logicGoal]] in
  (fst $ go nodes (Descend.Descend [logicGoal] []) env' Subst.empty Subst.empty, logicGoal, names) where
    go nodes d@(Descend.Descend goal ancs) env subst generalizer =
      -- if head (Env.getFreshNames env) > 10
      -- then (Prune d subst, nodes)
      -- else
        let subst = Subst.empty in
        -- let newNodes = (delete goal nodes) in
        let newNodes = filter (not . Embed.isVariant goal) nodes in

        let sldTree = LC.sldResolution goal env subst newNodes heuristic in
        let (substs, bodies) = partition (null . snd3) $ LC.resultants sldTree in

        if null bodies
        then
          (Node d Subst.empty sldTree [], nodes)
        else
          let ancs' = goal : ancs in
          let abstracted = map (abstractChild ancs') bodies in
          let (toUnfold, toNotUnfold, newNodes) =
                  foldl (\ (yes, no, seen) gs ->
                              let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (Embed.isVariant g) seen) gs in
                              -- let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (LC.isInst g) seen) gs in
                              (yes ++ brandNew, no ++ variants, map snd4 brandNew ++ seen)
                        )
                        ([], [], nodes)
                        abstracted
          in
          let (ch, everythingSeenSoFar) =
                  (swap . (reverse <$>) . swap) $
                  foldl (\(trees, seen) (subst, g, gen, env)  ->
                            let (t, s) = go seen (Descend.Descend g (goal : ancs)) env subst gen in
                            (t:trees, s)
                        )
                        ([], newNodes)
                        toUnfold in
          let forgetEnv = map (\(x, y, _) -> (x, y, Subst.empty)) in
          let forgetStuff = map (\(x, y, gen, _) -> (x,y, gen)) in
          let substLeaves = forgetEnv substs in
          let leaves = forgetStuff toNotUnfold in
          (Node d generalizer sldTree (map (\(subst, g, gen) -> Leaf (Descend.Descend g []) Subst.empty subst) (substLeaves ++ leaves) ++ ch), everythingSeenSoFar)
