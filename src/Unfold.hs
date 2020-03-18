module Unfold where

import           Data.Maybe  (mapMaybe)
import qualified Eval        as E
import           Syntax
import           Text.Printf (printf)

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
