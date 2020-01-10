module PartialDeduction where 

import Syntax 
import qualified Eval as E 
import qualified CPD
import Purification  
import Debug.Trace 
import Embed
import Miscellaneous (fst3, trd3)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Driving (generalizeGoals)
import Data.List (find)



data PDTree = Fail
            | Success E.Sigma
            | Or [PDTree] (CPD.Descend (G S)) E.Sigma 
            | Conj [PDTree] [G S] E.Sigma 
            | Gen PDTree (G S)
            | Leaf (G S)
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
      trace (printf "\nGo:\n%s\n" (show goal)) $ 
      let treeResult = 
            if any (isRenaming goal) seen 
            then
              Leaf goal 
            else 
              case find (`embed` goal) ancs of 
                Just g -> 
                  let ([newGoal], gen1, gen2, names) = generalizeGoals z [g] [goal] in 
                  let env' = (x, y, names) in
                  let (ch, _, _) = go (CPD.Descend newGoal ancs) env' seen state in 
                  trace (printf "\nCh\n%s\n" (show ch)) $ 
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
