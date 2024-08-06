module PD.PartialDeduction where

import           Control.Monad.State
import           Data.List           (find)
import           Data.Maybe          (mapMaybe)
import qualified Data.Set as Set
import qualified Descend
import           Embed
import qualified Environment         as Env
import qualified Eval                as E
import           Generalization      (generalizeGoals)
import           Prelude             hiding (or)
import           Program
import qualified Subst
import           Syntax
import           Unfold              (normalize, oneStepUnfold, unifyStuff)

data PDTree = Fail
            | Success (Subst.Subst S)
            | Or [PDTree] (Descend.Descend (G S)) (Subst.Subst S)
            | Conj [PDTree] [G S] (Subst.Subst S)
            | Gen PDTree (G S) (G S)
            | Leaf (G S) (Subst.Subst S) (G S)
            deriving (Show, Eq)

data PDState = PDState { getSeen :: Set.Set (G S), getEnv :: Env.Env }

modifySeen :: (Set.Set (G S) -> Set.Set (G S)) -> State PDState () 
modifySeen f = do 
  modify (\st -> PDState (f $ getSeen st) (getEnv st))

modifyEnv :: (Env.Env -> Env.Env) -> State PDState () 
modifyEnv f = do 
  modify (\st -> PDState (getSeen st) (f $ getEnv st))

topLevel :: Program G X -> (PDTree, G S, [S])
topLevel (Program defs goal) =
    let env = Env.fromDefs defs in
    let ((logicGoal, names), env') = runState (E.preEval goal) env in
    let descend = Descend.Descend logicGoal [] in
    let treeResult = evalState (go descend Subst.empty) (PDState Set.empty env') in 
    (treeResult, logicGoal, names)
  where
    go :: Descend.Descend (G S) -> Subst.Subst S -> State PDState PDTree 
    go d@(Descend.Descend goal ancs) subst = do 
      (PDState seen env) <- get
      case find (isVariant goal) (Set.toList seen) of 
        Just g ->
          return $ Leaf goal subst g 
        _ -> do 
          modifySeen (Set.insert goal)
          case find (`embed` goal) ancs of
            Just g -> do 
              let ([newGoal], gen1, gen2, names) = generalizeGoals (Env.getFreshNames env) [g] [goal] 
              let env' = Env.updateNames env names 
              modifyEnv (const env')
              ch <- go (Descend.Descend newGoal ancs) subst
              return $ Gen ch goal newGoal
            Nothing -> do 
              let (unfolded, env') = runState (oneStepUnfold goal) env 
              let normalized = normalize unfolded 
              let unified = mapMaybe (unifyStuff subst) normalized 
              modifyEnv (const env')
              ch <- mapM  (\(g, s') ->
                            if null g
                            then
                              return $ leaf s'
                            else do 
                              ch <- mapM (\h -> go (Descend.Descend h (goal : ancs)) s') g
                              return $ Conj ch  g s') 
                          unified
              return $ Or ch d subst

leaf :: Subst.Subst S -> PDTree
leaf x = if Subst.null x then Fail else Success x

simplify :: PDTree -> PDTree
simplify =
    go
  where
    go (Or    ch g s) = failOr ch (\x -> Or x g s)
    go (Conj  ch g s) = failConj ch (\x -> Conj x g s)
    go (Gen   ch g g') = failOr [ch] (\[x] -> Gen x g g')
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

restrictSubsts :: PDTree -> PDTree
restrictSubsts =
  go Subst.empty
  where
    go subst (Conj ch gs s) = Conj (map (go s) ch) gs (Subst.difference s subst)
    go subst (Or ch gs s) = Or (map (go s) ch) gs (Subst.difference s subst)
    go subst (Gen ch gs gs') = Gen (go subst ch) gs gs'
    go subst (Leaf gs s v) = Leaf gs (Subst.difference s subst) v
    go subst (Success s) = Success (Subst.difference s subst)
    go _ Fail = Fail
