module PartialDeduction where

import qualified CPD.LocalControl   as LC
import           Data.List          (find)
import           Data.Maybe         (mapMaybe)
import           Embed
import qualified Eval               as E
import           Generalization     (generalizeGoals)
import           Prelude            hiding (or)
import           Syntax
import           Unfold             (oneStepUnfold, unifyStuff, normalize)
import           Util.Miscellaneous (fst3)

data PDTree = Fail
            | Success E.MapSigma
            | Or [PDTree] (LC.Descend (G S)) E.MapSigma
            | Conj [PDTree] [G S] E.MapSigma
            | Gen PDTree (G S)
            | Leaf (G S) E.MapSigma
            | Split [PDTree] [G S] E.MapSigma
            deriving (Show, Eq)

topLevel :: Program -> (PDTree, G S, [S])
topLevel (Program defs goal) =
    let gamma = E.gammaFromDefs defs in
    let (logicGoal, gamma', names) = E.preEval gamma goal in
    let nodes = [] in
    let descend = LC.Descend logicGoal [] in
    go descend gamma' nodes E.s0
  where
    go d@(LC.Descend goal ancs) env@(x, y, z) seen state =
      let treeResult =
            if any (isRenaming goal) seen
            then
              Leaf goal state
            else
              case find (`embed` goal) ancs of
                Just g ->
                  let ([newGoal], gen1, gen2, names) = generalizeGoals z [g] [goal] in
                  let env' = (x, y, names) in
                  let (ch, _, _) = go (LC.Descend newGoal ancs) env' seen state in
                  Gen ch newGoal
                Nothing ->
                  let (unfolded, gamma) = oneStepUnfold goal env in
                  let normalized = normalize unfolded in
                  let unified = mapMaybe (unifyStuff state) normalized in
                  Or (map (\(g, s') ->
                        if null g
                        then
                          leaf s'
                        else
                          Conj (map (\h -> fst3 $ go (LC.Descend h (goal : ancs)) gamma (goal : seen) s') g) g s') unified)
                    d
                    state
      in (treeResult, V 1 === V 2, [4, 5, 6, 7])

leaf :: E.MapSigma -> PDTree
leaf x = if null x then Fail else Success x

simplify :: PDTree -> PDTree
simplify =
    go
  where
    go (Or    ch g s) = failOr ch (\x -> Or x g s)
    go (Conj  ch g s) = failConj ch (\x -> Conj x g s)
    go (Split ch g s) = failConj ch (\x -> Split ch g s)
    go (Gen   ch g  ) = failOr [ch] (\[x] -> Gen x g)
    go x              = x
    failOr ch f =
      let simplified = filter (/= Fail) $ map go ch in
      if null simplified
      then Fail
      else f simplified
    failConj ch f =
      let simplified = map go ch in
      if Fail `elem` simplified
      then Fail
      else f simplified

