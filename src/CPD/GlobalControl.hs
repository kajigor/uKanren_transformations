module CPD.GlobalControl where

import qualified CPD.LocalControl as LC
import Syntax
import Prelude hiding (sequence)
import Data.List (find, partition)
import qualified Eval as E
import Purification
import Text.Printf
import Debug.Trace
import qualified Tree as T
import Util.Miscellaneous
import Data.Tuple
import Embed 

type Descend = LC.Descend

data GlobalTree = Leaf  (Descend [G S]) T.Generalizer E.Sigma
                | Node  (Descend [G S]) T.Generalizer LC.SldTree [GlobalTree]
                | Prune (Descend [G S]) E.Sigma

sequence :: Descend a -> [a]
sequence = LC.getAncs

getNodes (Leaf _ _ _) = []
getNodes (Node _ _ (LC.Leaf _ _ _) _) = [] -- This happens because of the instance check
getNodes (Node d _ sld ch) = (LC.getCurr d, sld) : (concatMap getNodes ch)

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
  find (\b -> Embed.embed b m && (not (Embed.isVariant b m))) (sequence descend)

generalize :: [G S] -> [G S] -> E.Delta -> ([([G S], T.Generalizer)], E.Delta)
generalize m b d =
  let ((m1, m2), genOrig, delta) = LC.split d b m in -- TODO PCHM
  let genTrue = genOrig in
  --let (generalized, _, gen, delta') = D.generalizeGoals d m1 b in
  trace (printf "\nAfter split\n%s\n" (show' $ LC.mcs m1)) $  
  (map (project genTrue) (LC.mcs m1) ++ map (project []) (LC.mcs m2), delta)
  -- (map (project genTrue) [m1] ++ map (project []) (LC.mcs m2), delta)
    where
      project gen goals = (goals, {- filter (\(x, _) -> (V x) `elem` concatMap LC.vars goals) -} gen)

abstractChild :: [[G S]] -> (E.Sigma, [G S], Maybe E.Gamma) -> [(E.Sigma, [G S], T.Generalizer, E.Gamma)]
abstractChild _ (_, _, Nothing) = []
abstractChild ancs (subst, g, Just env@(x, y, d)) =
  let (abstracted, delta) = abstract (LC.Descend g ancs) g d in
  map (\(g, gen) -> (subst, g, gen, (x, y, delta))) abstracted

conjToList :: G a -> [G a]
conjToList (g :/\: h) = conjToList g ++ conjToList h
conjToList x@(Invoke _ _) = [x]
conjToList _ = error "This conjunction is not a list of calls"


topLevel :: Program -> (GlobalTree, G S, [S])
topLevel (Program defs goal) =
  -- let (goal', defs) = takeOutLets goal in
  let gamma = E.updateDefsInGamma E.env0 defs in
  let (logicGoal, gamma', names) = E.preEval gamma goal in
  let nodes = [[logicGoal]] in
  (fst $ go nodes (LC.Descend [logicGoal] []) gamma' E.s0 [], logicGoal, names) where
    go nodes d@(LC.Descend goal ancs) gamma subst generalizer =
      -- if head (trd3 gamma) > 10
      -- then (Prune d subst, nodes)
      -- else
        let subst = E.s0 in
        -- let newNodes = (delete goal nodes) in 
        let newNodes = filter (not . Embed.isVariant goal) nodes in 

        let sldTree = LC.sldResolution goal gamma subst newNodes in
        let (substs, bodies) = partition (null . snd3) $ LC.resultants sldTree in

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
                              let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (Embed.isVariant g) seen) gs in
                              -- let (variants, brandNew) = partition (\(_, g, _, _) -> null g || any (LC.isInst g) seen) gs in
                              (yes ++ brandNew, no ++ variants, map snd4 brandNew ++ seen)
                        )
                        ([], [], nodes)
                        abstracted
          in
          -- trace (printf "Here are the new nodes:\n%s\n" (show newNodes)) $
          let (ch, everythingSeenSoFar) =
                  (swap . (reverse <$>) . swap) $
                  foldl (\(trees, seen) (subst, g, gen, env)  ->
                            let (t, s) = go seen (LC.Descend g (goal : ancs)) env subst gen in
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
          (Node d generalizer sldTree (map (\(subst, g, gen) -> Leaf (LC.Descend g []) [] subst) (substLeaves ++ leaves) ++ ch), everythingSeenSoFar)
