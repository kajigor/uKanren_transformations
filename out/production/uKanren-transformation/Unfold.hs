module Unfold where

import           Control.Applicative
import           Control.Monad.State
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as Set
import           Def
import           Embed               (variantCheck)
import qualified Environment         as Env
import qualified Eval                as E
import qualified Subst
import           Syntax
import           Text.Printf         (printf)
import           Util.ListZipper
import           Util.Miscellaneous  (pinpoint)
import qualified VarInterpretation   as VI

oneStepUnfold :: G S -> State Env.Env (G S)
oneStepUnfold g@(Invoke f as) = do
  env <- get
  let (Def n fs body) = Env.getDef env f
  if length fs == length as
  then do
    let i' = foldl (\ interp (f, a) -> VI.extend interp f a) (Env.getInterp env) $ zip fs as
    let ((g', _), env') = runState (E.preEval body) (Env.updateInterp env i')
    put env'
    return g'
  else error $ printf "Unfolding error: different number of factual and actual arguments\nFactual: %s --- %s\nActual: %s --- %s)" f (show as) n (show fs)
oneStepUnfold g = return g

oneStep :: G S -> Subst.Subst -> State Env.Env [([G S], Subst.Subst)]
oneStep goal state = do
    unfolded <- oneStepUnfold goal
    let normalized = normalize unfolded
    let unified = mapMaybe (unifyStuff state) normalized
    return unified

unfoldConjunction :: [G S] -> Subst.Subst -> State Env.Env [([G S], Subst.Subst)]
unfoldConjunction (x:xs) state = do
  unified <- oneStep x state
  results <- mapM (\(g, s) -> do
                      goals <- unfoldConjunction xs s
                      return $ (\(x,y) -> (g ++ x, y)) <$> goals
                  ) unified
  return $ concat results
unfoldConjunction [] s = return [([],s)]

normalize :: G S -> [[G S]] -- disjunction of conjunctions of calls and unifications
normalize (Disjunction x y gs) = concatMap normalize (x : y : gs)
normalize (Conjunction x y gs) = (++) <$> normalize x <*> normalize (unsafeConj (y : gs))
normalize g@(Invoke _ _) = [[g]]
normalize g@(_ :=: _) = [[g]]
normalize (Delay g) = normalize g
normalize g = error ("Unexpected goal type in normalization\n" ++ show g)

unifyStuff :: Subst.Subst -> [G S] -> Maybe ([G S], Subst.Subst)
unifyStuff state gs =
    go gs state []
  where
    go [] state conjs = Just (reverse conjs, state)
    go (g@(Invoke _ _) : gs) state conjs = go gs state (g : conjs)
    go ((t :=: u) : gs) state conjs = do
      s <- E.unify  (Just state) t u
      go gs s conjs

maximumBranches :: Def G X -> Int
maximumBranches def@(Def _ args body) =
    let goal = fst $ evalState (E.preEval (fresh args body)) Env.empty in
    length $ evalState (oneStep (succeed goal) Subst.empty) Env.empty
  where
    succeed (Invoke name args) = success
    succeed (t :=: u) = t :=: u
    succeed (Conjunction x y g) = unsafeConj $ succeed <$> (x : y : g)
    succeed (Disjunction x y g) = unsafeDisj $ succeed <$> (x : y : g)
    succeed (Fresh name g) = Fresh name $ succeed g

    success = C "" [] :=: C "" []

getMaximumBranches :: Env.Env -> G S -> Int
getMaximumBranches env (Invoke name _) =
    let def = Env.getDef env name in
    maximumBranches def


notMaximumBranches :: Env.Env -> Subst.Subst -> G S -> Bool
notMaximumBranches env state goal@(Invoke name args) =
    let maxBranches = maximumBranches (Env.getDef env name) in
    let unfolded = evalState (oneStep goal state) env in
    length unfolded < maxBranches
    -- let result = length unfolded < maxBranches in
    -- result
notMaximumBranches _ _ _ = False

-- unfoldComplexity :: Env.Env -> E.Sigma -> G S -> Int
-- unfoldComplexity env@(p, _, _) state goal@(Invoke name args) =
--     let (unfolded, _) = oneStep goal env state in
--     (length $ filter (not . null . fst) unfolded)

-- unfoldComplexity :: Env.Env -> E.Sigma -> G S -> (Int, Int)
-- unfoldComplexity env@(p, _, _) state goal@(Invoke name args) =
--     let (unfolded, _) = oneStep goal env state in
--     (length $ filter (not . null . fst) unfolded, length unfolded)

data Complexity = Complexity { maxBranches :: Int, curBranches :: Int, substs :: Int }
                deriving (Eq, Show)

instance Ord Complexity where
  (Complexity max cur subst) <= (Complexity max' cur' subst') =
    cur < max || cur - subst >= cur' - subst' || cur <= cur'

findTupling :: Env.Env -> Subst.Subst -> [G S] -> [[G S]] -> Maybe (([([G S], Subst.Subst)], Env.Env))
findTupling env subst goal ancs =
    let (conjunctions, env') = runState (unfoldConjunction goal subst) env in
    -- let res@(unfolded, env') = runState (mapM (\g -> oneStep g subst) goal) env in
    -- let conjunctions = cross unfolded in
    if any (\(g, s) -> variantCheck (Subst.substitute s g) ancs) conjunctions
    then Just (conjunctions, env')
    else Nothing
  where
    cross :: [[([G S], Subst.Subst)]] -> [([G S], Subst.Subst)]
    cross [] = [([],Subst.empty)]
    cross (x:xs) = [ (y ++ z, Subst.union s' s'') | (y, s') <- x, (z, s'') <- cross xs ]



findBestByComplexity :: Env.Env -> Subst.Subst -> [G S] -> Maybe (Zipper (G S))
findBestByComplexity env sigma goals =
    let estimated = map (\g -> (g, unfoldComplexity env sigma g)) goals in
    pinpoint (\(Invoke name _) -> static env name) goals
    <|> throwAwayComplexity (onlySubsts estimated
                            <|> deterministic estimated
                            <|> maxBranch estimated
                            <|> partialSubst estimated)
  where
    onlySubsts xs = pinpoint (\(g, compl) -> curBranches compl == substs compl) xs
    deterministic = pinpoint (\(g, compl) -> curBranches compl == 1)
    maxBranch  xs = pinpoint (\(g, compl) -> maxBranches compl > curBranches compl) xs
    partialSubst xs = pinpoint (\(g, compl) -> substs compl > 0) xs
    throwAwayComplexity z = (fst <$>) <$> z

unfoldComplexity :: Env.Env -> Subst.Subst -> G S -> Complexity
unfoldComplexity env sigma goal@(Invoke name _) =
  let unfolded = evalState (oneStep goal sigma) env in
  let max = maximumBranches (Env.getDef env name) in
  Complexity max (length unfolded) (length $ filter (null . fst) unfolded)

static :: Env.Env -> String -> Bool
static env name =
    go (Set.fromList [name]) (getBody name)
  where
    go set (Invoke name _) | Set.member name set = False
    go set (Invoke name _) = go (Set.insert name set) (getBody name)
    go set (Conjunction x y gs) = all (go set) (x : y : gs)
    go set (Disjunction x y gs) = all (go set) (x : y : gs)
    go set (Fresh _ g) = go set g
    go _ _ = True

    getBody name =
      let Def _ _ body = Env.getDef env name in body

isGoalStatic :: Env.Env -> G S -> Bool
isGoalStatic env (Invoke name _) =
  static env name
isGoalStatic _ _ = False
