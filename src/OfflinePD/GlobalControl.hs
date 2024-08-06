{-# LANGUAGE TupleSections #-}

module OfflinePD.GlobalControl where

import           Control.Monad.State (runState)
import           BTA.InvokeAnnotation
import           BTA.AnnotatedProgram
import           Descend
import           Generalization (Generalizer)
import           Syntax
import           OfflinePD.Generalization
import           Printer.AnnSldTree
import qualified Embed
import qualified OfflinePD.AnnEnvironment as Env
import qualified OfflinePD.AnnotatedEval as E
import qualified OfflinePD.LocalControl as LC
import qualified FreshNames          as FN
import qualified Subst
import qualified CPD.LocalControl as LCcpd

import           Data.List           (find, partition)
import           Data.Tuple
import           Prelude             hiding (sequence)
import           Util.Miscellaneous
import           Printer.Dot
import Debug.Trace (traceShow, trace)


data GlobalTree = Leaf  (Descend [AnnG Term S]) Generalizer (Subst.Subst S)
                | Node  (Descend [AnnG Term S]) Generalizer LC.SldTree [GlobalTree]
                | Prune (Descend [AnnG Term S]) (Subst.Subst S)
                deriving (Show)

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
part :: [AnnG Term S] -> [[AnnG Term S]]
part = LC.mcs
--
abstract :: Descend [AnnG Term S] -> [AnnG Term S] -> FN.FreshNames -> ([([AnnG Term S], Generalizer)], FN.FreshNames)
abstract descend goals d =
  let qCurly = part -- $trace ("abstract" ++ show goals ++ show (getCurr descend) ++ show (getAncs descend))
                    goals in
  let result = go (map (, Subst.empty) qCurly) -- $ trace ("Curly" ++ show qCurly ++ "\n") 
                                              d in
  --traceShow result 
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

whistle :: Descend [AnnG Term S] -> [AnnG Term S] -> Maybe [AnnG Term S]
whistle descend m =
  find (\b -> Embed.embed b m && not (Embed.isVariant b m)) (sequence descend) --trace ("whistle " ++ show b ++ show m ++ show (Embed.embed b m) ++ show (not (Embed.isVariant b m))) 

generalize :: [AnnG Term S] -> [AnnG Term S] -> FN.FreshNames -> ([([AnnG Term S], Generalizer)], FN.FreshNames)
generalize m b d =
  let ((m1, m2), genOrig, delta) = LC.split d b m in -- TODO PCHM
  let genTrue = genOrig in
  --let (generalized, _, gen, delta') = D.generalizeGoals d m1 b in
  (map (project genTrue) (LC.mcs m1) ++ map (project Subst.empty) (LC.mcs m2), delta)
  -- (map (project genTrue) [m1] ++ map (project []) (LC.mcs m2), delta)
    where
      project gen goals = (goals, {- filter (\(x, _) -> (V x) `elem` concatMap LC.vars goals) -} gen)

abstractChild :: [[AnnG Term S]] -> (Subst.Subst S, [AnnG Term S], Maybe Env.Env) -> [(Subst.Subst S, [AnnG Term S], Generalizer, Env.Env)]
abstractChild _ (_, _, Nothing) = []
abstractChild ancs (subst, g, Just env) =
  let (abstracted, d') = abstract (Descend.Descend g ancs) g  -- $ trace ("abstractChild " ++ show g ++ show ancs ++ show subst ) 
                                                              (Env.getFreshNames env) in
  map (\(g, gen) -> (subst, g, gen, Env.updateNames env d')) -- $ traceShow abstracted 
                                                              abstracted

topLevel :: AnnotatedProgram (AnnG Term) X -> LCcpd.Heuristic -> (GlobalTree, AnnG Term S, [S])
topLevel (AnnotatedProgram defs goal) heu =
  let env = Env.fromDefs defs in
  let ((logicGoal, names), env') = runState (E.preEval goal) env in
  let nodes = [LC.conjToList logicGoal] in
  (fst $ go nodes (Descend.Descend [logicGoal] []) env' Subst.empty Subst.empty, logicGoal, names) where
    go nodes d@(Descend.Descend goal ancs) env subst generalizer =
--       if head (Env.getFreshNames env) > 10
--        then (Prune d subst, nodes)
--       else
        let subst = Subst.empty in
        -- let newNodes = (delete goal nodes) in
        let newNodes = filter (not . Embed.isVariant goal) $ traceShow goal 
                        nodes in

        let sldTree = LC.sldResolution (trace ("Global" ++ show d) goal) env subst newNodes heu in
        let (substs, bodies) = partition (null . snd3) $ LC.resultants -- $ traceShow d  
                               sldTree in
          
--        run printTree (path </> "local1.dot") sldTree

        if null bodies
        then
          (Node d Subst.empty sldTree [], nodes)
        else
          let ancs' = goal : ancs in
          let abstracted = map (abstractChild ancs') -- $ traceShow bodies
                            bodies in  
          let (toUnfold, toNotUnfold, newNodes) =
                  foldl (\ (yes, no, seen) gs ->
                              let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (Embed.isVariant g) seen) gs in
                              -- let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (LC.isInst g) seen) gs in
                              (yes ++ brandNew, no ++ variants, map snd4 brandNew ++ seen)
                        )
                        ([], [], nodes) -- $ traceShow ("Abstracted" ++ show (zip bodies abstracted))
                          abstracted
          in
          let (ch, everythingSeenSoFar) =
                  (swap . (reverse <$>) . swap) $
                  foldl (\(trees, seen) (subst, g, gen, env)  ->
                            let (t, s) = go seen (Descend.Descend g (goal : ancs)) env subst gen in
                            (t:trees, s)
                        )
                        ([], newNodes) -- $ traceShow ("toUnfold" ++ show toUnfold) 
                          toUnfold 
          in
          let forgetEnv = map (\(x, y, _) -> (x, y, Subst.empty)) in
          let forgetStuff = map (\(x, y, gen, _) -> (x,y, gen)) in
          let substLeaves = forgetEnv substs in
          let leaves = forgetStuff toNotUnfold in
          (Node d generalizer sldTree (map (\(subst, g, gen) -> Leaf (Descend.Descend g []) Subst.empty subst) (substLeaves ++ leaves) ++ ch), everythingSeenSoFar)
