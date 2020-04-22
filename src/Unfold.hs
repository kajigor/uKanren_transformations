module Unfold where

import           Data.Maybe         (mapMaybe)
import qualified Eval               as E
import           Syntax
import           Text.Printf        (printf)
import           Util.Miscellaneous (fst3, show')
import Debug.Trace (trace)

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

unfoldComplexity :: E.Gamma -> E.Sigma -> G S -> (Int, Int)
unfoldComplexity gamma@(p, _, _) state goal@(Invoke name args) =
    let (unfolded, _) = oneStep goal gamma state in
    trace (printf "\nUnfolded:\nGoal: %s\n%s\n" (show goal) (show' unfolded)) $
    (length $ filter (not . null . fst) unfolded, length unfolded)