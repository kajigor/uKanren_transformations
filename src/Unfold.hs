module Unfold where

import           Control.Applicative
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as Set
import qualified Eval                as E
import qualified Subst
import           Syntax
import           Text.Printf         (printf)
import           Util.Miscellaneous  (fst3, pinpoint)
import qualified VarInterpretation   as VI
import qualified Definitions         as Defs

oneStepUnfold :: G S -> E.Gamma -> (G S, E.Gamma)
oneStepUnfold g@(Invoke f as) env@(p, i, d) =
  let (Def n fs body) = Defs.getDef p f in
  if length fs == length as
  then
    let i' = foldl (\ interp (f, a) -> VI.extend interp f a) i $ zip fs as in
    let (g', env', _) = E.preEval (p, i', d) body in
    (g', env')
  else error $ printf "Unfolding error: different number of factual and actual arguments\nFactual: %s --- %s\nActual: %s --- %s)" f (show as) n (show fs)
oneStepUnfold g env = (g, env)

oneStep :: G S -> E.Gamma -> Subst.Subst -> ([([G S], Subst.Subst)], E.Gamma)
oneStep goal env state =
    let (unfolded, gamma) = oneStepUnfold goal env in
    let normalized = normalize unfolded in
    let unified = mapMaybe (unifyStuff state) normalized in
    (unified, gamma)

normalize :: G S -> [[G S]] -- disjunction of conjunctions of calls and unifications
normalize (f :\/: g) = normalize f ++ normalize g
normalize (f :/\: g) = (++) <$> normalize f <*> normalize g
normalize g@(Invoke _ _) = [[g]]
normalize g@(_ :=: _) = [[g]]
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

maximumBranches :: Def -> Int
maximumBranches def@(Def _ args body) =
    let goal = fst3 $ E.preEval E.env0 (fresh args body) in
    length $ fst $ oneStep (succeed goal) E.env0 Subst.empty
  where
    succeed (g :/\: h)         = succeed g :/\: succeed h
    succeed (g :\/: h)         = succeed g :\/: succeed h
    succeed (Fresh name g)     = Fresh name (succeed g)
    succeed (Invoke name args) = success
    succeed (t :=: u)          = t :=: u

    success = C "" [] :=: C "" []

getMaximumBranches :: E.Gamma -> G S -> Int
getMaximumBranches (p,_,_) (Invoke name _) =
    let def = Defs.getDef p name in
    maximumBranches def


notMaximumBranches :: E.Gamma -> Subst.Subst -> G S -> Bool
notMaximumBranches gamma@(p, _, _) state goal@(Invoke name args) =
    let maxBranches = maximumBranches (Defs.getDef p name) in
    let (unfolded, _) = oneStep goal gamma state in
    length unfolded < maxBranches
    -- let result = length unfolded < maxBranches in
    -- trace (printf "\nGoal: %s\nNot maximum branches: %s\nUnfoldComplexity: %s\nUnfolded: %s\nMaxBranches: %s\n" (show goal) (show result) (show $ length $ filter (not . null . fst) unfolded) (show $ length unfolded) (show maxBranches)) $
    -- result
notMaximumBranches _ _ _ = False

-- unfoldComplexity :: E.Gamma -> E.Sigma -> G S -> Int
-- unfoldComplexity gamma@(p, _, _) state goal@(Invoke name args) =
--     let (unfolded, _) = oneStep goal gamma state in
--     (length $ filter (not . null . fst) unfolded)

-- unfoldComplexity :: E.Gamma -> E.Sigma -> G S -> (Int, Int)
-- unfoldComplexity gamma@(p, _, _) state goal@(Invoke name args) =
--     let (unfolded, _) = oneStep goal gamma state in
--     trace (printf "\nUnfolded:\nGoal: %s\n%s\n" (show goal) (show' unfolded)) $
--     (length $ filter (not . null . fst) unfolded, length unfolded)

data Complexity = Complexity { maxBranches :: Int, curBranches :: Int, substs :: Int }
                deriving (Eq, Show)

instance Ord Complexity where
  (Complexity max cur subst) <= (Complexity max' cur' subst') =
    cur < max || cur - subst >= cur' - subst' || cur <= cur'

findBestByComplexity :: E.Gamma -> Subst.Subst -> [G S] -> Maybe ([G S], G S, [G S])
findBestByComplexity gamma sigma goals =
    let estimated = map (\g -> (g, unfoldComplexity gamma sigma g)) goals in
    pinpoint (\(Invoke name _) -> static gamma name) goals
    <|> throwAwayComplexity (onlySubsts estimated
                             <|> deterministic estimated
                             <|> maxBranch estimated
                             <|> partialSubst estimated)
  where
    onlySubsts xs = pinpoint (\(g, compl) -> curBranches compl == substs compl) xs
    deterministic = pinpoint (\(g, compl) -> curBranches compl == 1)
    maxBranch  xs = pinpoint (\(g, compl) -> maxBranches compl > curBranches compl) xs
    partialSubst xs = pinpoint (\(g, compl) -> substs compl > 0) xs
    throwAwayComplexity input = do
      (ls, x, rs) <- input
      return (map fst ls, fst x, map fst rs)

unfoldComplexity :: E.Gamma -> Subst.Subst -> G S -> Complexity
unfoldComplexity gamma@(p, _, _) sigma goal@(Invoke name _) =
  let (unfolded, _) = oneStep goal gamma sigma in
  let max = maximumBranches (Defs.getDef p name) in
  -- trace (printf "\n%s is %s\n" name (if static gamma name then "static" else "not static")) $
  Complexity max (length unfolded) (length $ filter (null . fst) unfolded)

static :: E.Gamma -> String -> Bool
static (p, _, _) name =
    go (Set.fromList [name]) (getBody name)
  where
    go set (Invoke name _) | Set.member name set = False
    go set (Invoke name _) = go (Set.insert name set) (getBody name)
    go set (g :/\: h) = go set g && go set h
    go set (g :\/: h) = go set g && go set h
    go set (Fresh _ g) = go set g
    go _ _ = True

    getBody name =
      let Def _ _ body = Defs.getDef p name in body

isGoalStatic :: E.Gamma -> G S -> Bool
isGoalStatic gamma (Invoke name _) =
  static gamma name
isGoalStatic _ _ = False