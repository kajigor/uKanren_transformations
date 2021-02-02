module PartialDeduction where

import qualified CPD.LocalControl   as LC
import           Data.List          (find)
import           Data.Maybe         (mapMaybe)
import qualified Descend
import           Embed
import qualified Eval               as E
import           Generalization     (generalizeGoals)
import           Prelude            hiding (or)
import qualified Subst
import           Syntax
import           Unfold             (oneStepUnfold, unifyStuff, normalize)
import           Util.Miscellaneous (fst3)
import qualified Environment as Env

data PDTree = Fail
            | Success Subst.Subst
            | Or [PDTree] (Descend.Descend (G S)) Subst.Subst
            | Conj [PDTree] [G S] Subst.Subst
            | Gen PDTree (G S)
            | Leaf (G S) Subst.Subst
            | Split [PDTree] [G S] Subst.Subst
            deriving (Show, Eq)

topLevel :: Program -> (PDTree, G S, [S])
topLevel (Program defs goal) =
    let env = Env.fromDefs defs in
    let (logicGoal, env', _) = E.preEval env goal in
    let nodes = [] in
    let descend = Descend.Descend logicGoal [] in
    go descend env' nodes Subst.empty
  where
    go d@(Descend.Descend goal ancs) env seen subst =
      let treeResult =
            if any (isRenaming goal) seen
            then
              Leaf goal subst
            else
              case find (`embed` goal) ancs of
                Just g ->
                  let ([newGoal], gen1, gen2, names) = generalizeGoals (Env.getFreshNames env) [g] [goal] in
                  let env' = Env.updateNames env names in
                  let (ch, _, _) = go (Descend.Descend newGoal ancs) env' seen subst in
                  Gen ch newGoal
                Nothing ->
                  let (unfolded, env') = oneStepUnfold goal env in
                  let normalized = normalize unfolded in
                  let unified = mapMaybe (unifyStuff subst) normalized in
                  Or (map (\(g, s') ->
                        if null g
                        then
                          leaf s'
                        else
                          Conj (map (\h -> fst3 $ go (Descend.Descend h (goal : ancs)) env' (goal : seen) s') g) g s') unified)
                    d
                    subst
      in (treeResult, V 1 === V 2, [4, 5, 6, 7])

leaf :: Subst.Subst -> PDTree
leaf x = if Subst.null x then Fail else Success x

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

