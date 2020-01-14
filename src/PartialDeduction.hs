module PartialDeduction where 

import Syntax 
import qualified Eval as E 
import qualified CPD
import Purification  
import Embed
import Util.Miscellaneous (fst3, show')
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Driving (generalizeGoals)
import Data.List (find, partition)
import qualified Data.Map.Strict as M 
import Debug.Trace (trace)

data PDTree = Fail
            | Success E.Sigma
            | Or [PDTree] (CPD.Descend (G S)) E.Sigma 
            | Conj [PDTree] [G S] E.Sigma 
            | Gen PDTree (G S)
            | Leaf (G S) E.Sigma
            deriving (Show, Eq)

topLevel :: G X -> (PDTree, G S, [S])
topLevel goal = 
    let (goal', defs) = takeOutLets goal in
    let gamma = E.updateDefsInGamma E.env0 defs in
    let (logicGoal, gamma', names) = E.preEval' gamma goal' in
    let nodes = [] in 
    let descend = CPD.Descend logicGoal [] in 
    go descend gamma' nodes E.s0 
  where 
    go d@(CPD.Descend goal ancs) env@(x, y, z) seen state = 
      let treeResult = 
            if any (isRenaming goal) seen 
            then
              Leaf goal state
            else 
              case find (`embed` goal) ancs of 
                Just g -> 
                  let ([newGoal], gen1, gen2, names) = generalizeGoals z [g] [goal] in 
                  let env' = (x, y, names) in
                  let (ch, _, _) = go (CPD.Descend newGoal ancs) env' seen state in 
                  Gen ch newGoal
                Nothing -> 
                  let (unfolded, gamma) = CPD.oneStepUnfold goal env in 
                  let normalized = CPD.normalize unfolded in 
                  let unified = mapMaybe (CPD.unifyStuff state) normalized in
                  Or (map (\(g, s') -> 
                        if null g 
                        then 
                          if null s' 
                          then Fail 
                          else Success s'
                        else 
                          Conj (map (\h -> fst3 $ go (CPD.Descend h (goal : ancs)) gamma (goal : seen) s') g) g s') unified) 
                    d
                    state 
      in (treeResult, V 1 === V 2, [4, 5, 6, 7])

nonConjunctive :: G X -> (PDTree, G S, [S])
nonConjunctive goal = 
    let (goal', defs) = takeOutLets goal in
    let gamma = E.updateDefsInGamma E.env0 defs in
    let (logicGoal, gamma', names) = E.preEval' gamma goal' in
    let nodes = [] in 
    let descend = CPD.Descend logicGoal [] in 
    go descend gamma' nodes E.s0 
  where 
    go d@(CPD.Descend goal ancs) env@(x, y, z) seen state = 
      let treeResult = 
            if any (isRenaming goal) seen 
            then
              Leaf goal state
            else 
              case find (`embed` goal) ancs of 
                Just g -> 
                  let ([newGoal], gen1, gen2, names) = generalizeGoals z [g] [goal] in 
                  let env' = (x, y, names) in
                  let (ch, _, _) = go (CPD.Descend newGoal ancs) env' seen state in 
                  Gen ch newGoal
                Nothing -> 
                  let (unfolded, gamma) = CPD.oneStepUnfold goal env in 
                  let normalized = CPD.normalize unfolded in 
                  let unified = mapMaybe (CPD.unifyStuff state) normalized in
                  
                  trace (printf "\nIn go\ngoal: %s\nstate: %s\n" (show goal) (show state)) $
                  let helpful = findConflicting $ map snd unified in 
                  trace (printf "\nConflicting:\n%s\n" (show' helpful)) $

                  Or (map (\(g, s') -> 
                        if null g 
                        then 
                          if null s' 
                          then Fail 
                          else Success s'
                        else 
                          let ch = map (\h -> fst3 $ go (CPD.Descend h (goal : ancs)) gamma (goal : seen) s') g in 
                          let substs = map collectSubsts ch in 
                          trace (printf "\nCollected substs\n%s\n" (show' substs)) $ 
                          Conj ch g s') unified) 
                    d
                    state 
      in (treeResult, V 1 === V 2, [4, 5, 6, 7])

collectSubsts :: PDTree -> [E.Sigma]
collectSubsts (Or ch _ _) = 
    mapMaybe go ch 
  where 
    go Fail = Nothing
    go (Success s) = Just s
    go (Or _ _ s) = Just s 
    go (Conj _ _ s) = Just s
    go (Gen _ _) = Nothing 
    go (Leaf _ s) = Just s 
    go _ = trace "something wrong in go" $ Nothing 
collectSubsts x = trace (printf "\nPattern matching Failed on\n%s\n" $ show x) $ []
collectSubsts _ = error "wat"

isConflicting :: E.Sigma -> E.Sigma -> Bool 
isConflicting s1 s2 = 
  let m1 = M.fromList s1 in 
  let m2 = M.fromList s2 in 
  let intersection = M.intersectionWith (\x y -> (E.walk x s1, E.walk y s2)) m1 m2 in
  M.size intersection > 0 && 
  any (uncurry (/=)) intersection

findConflicting :: [E.Sigma] -> [[E.Sigma]]
findConflicting [] = [] 
findConflicting (x:xs) = 
    go [x] [] xs 
  where
    go [] [] [] = [] 
    go [] conf [] = [reverse conf]
    go [] conf unConf = (reverse conf : findConflicting unConf)
    go (x:xs) conf unConf = 
      let (conf', unConf') = partition (isConflicting x) unConf in 
      go (xs ++ conf') (x : conf) unConf' 