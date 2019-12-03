module GlobalControl where

import qualified CPD
import Syntax
import Prelude hiding (sequence)
import Data.Maybe (isJust)
import Data.List (find, partition, inits, intercalate)
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
import Control.Exception.Base
import Data.Tuple
import Data.List

type Descend = CPD.Descend

data GlobalTree = Leaf  (Descend [G S]) T.Generalizer E.Sigma
                | Node  (Descend [G S]) T.Generalizer CPD.SldTree [GlobalTree]
                | Prune (Descend [G S]) E.Sigma

sequence :: Descend a -> [a]
sequence = CPD.getAncs

getNodes (Leaf _ _ _) = []
getNodes (Node _ _ (CPD.Leaf _ _ _) _) = [] -- This happens because of the instance check
getNodes (Node d _ sld ch) = (CPD.getCurr d, sld) : (concatMap getNodes ch)

{- branch :: GlobalTree -> Set [G S]
branch (Leaf d _ _) = sequence d
branch (Node d _ _ _) = sequence d

leaves :: GlobalTree -> Set [G S]
leaves (Leaf d _ _ ) = Set.singleton $ CPD.getCurr d
leaves (Node _ _ _ ch) =
  let sets = map leaves ch in
  foldr Set.union Set.empty sets -}

-- initial splitting into maximally connected suconjunctions, may be something else
part :: [G S] -> [[G S]]
part = CPD.mcs

abstract :: Descend [G S] -> [G S] -> E.Delta -> ([([G S], T.Generalizer)], E.Delta)
abstract descend goals d =
  let qCurly = part goals in
  let result = go (map (\x -> (x, [])) qCurly) d in 
  result 
   where
    go [] d@(x:_) = ([], d)
    go ((m, gen):gs) d =
      case whistle descend m of
        Nothing ->
          let (goals, delta) = go gs d in
          ((m, gen) : goals, delta)
        Just b ->
          let (goals, delta) = generalize m b d in
          trace ("generalize done " ++ show' goals) $
          let (blah, halb) = go gs delta in 
          (goals ++ blah, halb)

whistle :: Descend [G S] -> [G S] -> Maybe [G S]
whistle descend m =
  find (\b -> CPD.embed b m && (not (CPD.isVariant b m))) (sequence descend)

generalize :: [G S] -> [G S] -> E.Delta -> ([([G S], T.Generalizer)], E.Delta)
generalize m b d =
  let ((m1, m2), genOrig, delta) = CPD.split d b m in -- TODO PCHM
  let genTrue = genOrig in
  --let (generalized, _, gen, delta') = D.generalizeGoals d m1 b in
  trace (printf "\nAfter split\n%s\n" (show' $ CPD.mcs m1)) $  
  (map (project genTrue) (CPD.mcs m1) ++ map (project []) (CPD.mcs m2), delta)
  -- (map (project genTrue) [m1] ++ map (project []) (CPD.mcs m2), delta)
    where
      project gen goals = (goals, {- filter (\(x, _) -> (V x) `elem` concatMap CPD.vars goals) -} gen)

abstractChild :: [[G S]] -> (E.Sigma, [G S], Maybe E.Gamma) -> [(E.Sigma, [G S], T.Generalizer, E.Gamma)]
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
  (fst $ go nodes (CPD.Descend [logicGoal] []) gamma' E.s0 [], logicGoal, names) where
    go nodes d@(CPD.Descend goal ancs) gamma subst generalizer =
      -- if head (trd3 gamma) > 10
      -- then (Prune d subst, nodes)
      -- else
        let subst = E.s0 in
        -- let newNodes = (delete goal nodes) in 
        let newNodes = filter (not . CPD.isVariant goal) nodes in 

        let sldTree = CPD.sldResolution goal gamma subst newNodes in
        let (substs, bodies) = partition (null . snd3) $ CPD.resultants sldTree in

        if null bodies 
        then 
          (Node d [] sldTree [], nodes)
        else
          let ancs' = goal : ancs in 
          let abstracted = map (abstractChild ancs') bodies in
          trace (printf "\nbodies:\n%s\n" (show' $ map snd3 bodies)) $ 
          trace (printf "\nancs':\n%s\n" (show' ancs'))
          trace (printf "\nAbstracted\n%s" (show' $ map  (map snd4) abstracted)) $ 
          let (toUnfold, toNotUnfold, newNodes) =
                  foldl (\ (yes, no, seen) gs ->
                              let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (CPD.isVariant g) seen) gs in
                              -- let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (CPD.isInst g) seen) gs in
                              (yes ++ brandNew, no ++ variants, map snd4 brandNew ++ seen)
                        )
                        ([], [], nodes)
                        abstracted
          in
          -- trace (printf "Here are the new nodes:\n%s\n" (show newNodes)) $
          let (ch, everythingSeenSoFar) =
                  (swap . (reverse <$>) . swap) $
                  foldl (\(trees, seen) (subst, g, gen, env)  ->
                            let (t, s) = go seen (CPD.Descend g (goal : ancs)) env subst gen in
                            -- trace (printf "\nprocessing\n%s\nseen\n%s\n" (show g) (intercalate "\n" $ map show seen)) $
                            (t:trees, s)
                        )
                        ([], newNodes)
                        toUnfold in
          -- trace (printf "Everything we've seen so far:\n%s\n" (show everythingSeenSoFar)) $                        
          let forgetEnv = map (\(x, y, _) -> (x, y, [])) in
          let forgetStuff = map (\(x, y, gen, _) -> (x,y, gen)) in
          let substLeaves = forgetEnv substs in
          let leaves = forgetStuff toNotUnfold in
          (Node d generalizer sldTree (map (\(subst, g, gen) -> Leaf (CPD.Descend g []) [] subst) (substLeaves ++ leaves) ++ ch), everythingSeenSoFar)










-- topLevel :: G X -> (GlobalTree, G S, [S])
-- topLevel goal =
--   let (goal', defs) = takeOutLets goal in
--   let gamma = E.updateDefsInGamma E.env0 defs in
--   let (logicGoal, gamma', names) = E.preEval' gamma goal' in
--   let nodes = [[logicGoal]] in
--   (fst $ go nodes (CPD.Descend [logicGoal] Set.empty) gamma' E.s0 [] [], logicGoal, names) where
--     go nodes d@(CPD.Descend goal ancs) gamma subst defs generalizer =
--       if head (trd3 gamma) <= 400
--       then
--         trace (printf "GlobalLevel:\n%s\n" $ show goal) $
--         let subst = E.s0 in
--         let sldTree = CPD.sldResolution goal gamma subst (Set.delete goal $ Set.fromList nodes)in
--         -- trace (printf "\n\nSLDDDD\n%s\n\n%s\n\n\n" (show goal) $ simplyPrintTree sldTree) $
--         let (substs, bodies) = partition (null . snd3) $ CPD.resultants sldTree in
--         let abstracted = map (abstractChild ancs) bodies in
--         trace (printf "\nResultants:\n%s\nAbstracted:\n%s\n" (intercalate "\n" $ map (show . snd3) bodies) (intercalate "\n" $ map (concatMap (show . trd4)) abstracted) ) $
--         let (toUnfold, toNotUnfold, newNodes) =
--                 foldl (\ (yes, no, seen) gs ->
--                             -- let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (CPD.isVariant g) seen) gs in
--                             let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (CPD.isInst g) seen) gs in
--                             trace (printf "\n\nVARIANTS\n%s\n%s\n" (show $ map snd4 gs) (show $ map snd4 variants)) $
--                             (yes ++ brandNew, no ++ variants, map snd4 brandNew ++ seen)
--                       )
--                       ([], [], nodes)
--                       abstracted
--             in
--         -- let leafGoals = map snd3 toUnfold in
--         let (def, newDefs) = undefined in
--         let (ch, everythingSeenSoFar) =
--                 foldl (\(trees, seen) (subst, g, gen, env)  ->
--                           trace (printf "\n\nGlobalLevel:\n%s\n\n" $ show g) $
--                           trace (printf "\n\nSeen:\n%s\n\n" $ show seen) $
--                           --assert (g `notElem` seen) $
--                           let (t, s) = go seen (CPD.Descend g (Set.insert goal ancs)) env subst newDefs gen in
--                           (t:trees, s)
--                       )
--                       ([], newNodes)
--                       toUnfold in
--         let forgetEnv = map (\(x, y, _) -> (x, y, [])) in
--         let forgetStuff = map (\(x, y, gen, _) -> (x,y, gen)) in
--         let substLeaves = forgetEnv substs in
--         let leaves = forgetStuff toNotUnfold in
--         (Node d generalizer sldTree (map (\(subst, g, gen) -> Leaf (CPD.Descend g Set.empty) [] subst) (substLeaves ++ leaves) ++ ch), everythingSeenSoFar)
--       else
--         (Prune d subst, nodes)

      -- let ch = map (\((subst, g, env), ns) ->
      --                        go (ns ++ leafGoals ++ nodes) (CPD.Descend g (goal:ancs)) env subst) (zip qs (map (map snd3) $ inits qs)) in -- add qs where appropriate
      --       Node d (map (\(subst, g, _) -> Leaf (CPD.Descend g []) subst) chToNotUnfold ++ ch)
      --     else Prune d subst
