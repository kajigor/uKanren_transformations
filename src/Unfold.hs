module Unfold where

import           Control.Applicative
import           Data.List           (find)
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import qualified Eval                as E
import           Syntax
import           Text.Printf         (printf)
import           Util.Miscellaneous  (fst3, show', pinpoint)

oneStepUnfold :: G S -> E.Gamma -> (G S, E.Gamma)
oneStepUnfold g@(Invoke f as) env@(p, i, d) =
  let (Def n fs body) = p f in
  if length fs == length as
  then
    let i' = foldl (\ interp (f, a) -> E.extend interp f a) i $ zip fs as in
    let (g', env', _) = E.preEval (p, i', d) body in
    (g', env')
  else error $ printf "Unfolding error: different number of factual and actual arguments\nFactual: %s --- %s\nActual: %s --- %s)" f (show as) n (show fs)
oneStepUnfold g env = (g, env)

oneStep :: G S -> E.Gamma -> E.Sigma -> ([([G S], E.Sigma)], E.Gamma)
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

unifyStuff :: E.Sigma -> [G S] -> Maybe ([G S], E.Sigma)
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
    length $ fst $ oneStep (succeed goal) E.env0 E.s0
  where
    succeed (g :/\: h)         = succeed g :/\: succeed h
    succeed (g :\/: h)         = succeed g :\/: succeed h
    succeed (Fresh name g)     = Fresh name (succeed g)
    succeed (Invoke name args) = success
    succeed (t :=: u)          = t :=: u
    succeed x                  = error ("Failed to transform " ++ show x)

    success = C "" [] :=: C "" []

notMaximumBranches :: E.Gamma -> E.Sigma -> G S -> Bool
notMaximumBranches gamma@(p, _, _) state goal@(Invoke name args) =
    let maxBranches = maximumBranches (p name) in
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

findBestByComplexity :: E.Gamma -> E.Sigma -> [G S] -> Maybe ([G S], G S, [G S])
findBestByComplexity gamma sigma goals =
    let estimated = map (\g -> (g, unfoldComplexity gamma sigma g)) goals in
    pinpoint (\(Invoke name _) -> static gamma name) goals
    <|> throwAwayComplexity (onlySubsts estimated
                             <|> maxBranch estimated
                             <|> partialSubst estimated)
  where
    onlySubsts xs = pinpoint (\(g, compl) -> curBranches compl == substs compl) xs
    maxBranch  xs = pinpoint (\(g, compl) -> maxBranches compl > curBranches compl) xs
    partialSubst xs = pinpoint (\(g, compl) -> substs compl > 0) xs
    throwAwayComplexity input = do
      (ls, x, rs) <- input
      return (map fst ls, fst x, map fst rs)

unfoldComplexity :: E.Gamma -> E.Sigma -> G S -> Complexity
unfoldComplexity gamma@(p, _, _) sigma goal@(Invoke name _) =
  let (unfolded, _) = oneStep goal gamma sigma in
  let max = maximumBranches (p name) in
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
      let Def _ _ body = p name in body