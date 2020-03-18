{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections          #-}

module Generalization where

import           Control.Exception.Base
import           Data.List              hiding (group, groupBy)
import qualified Data.Map               as Map
import           Embed
import qualified Eval                   as E
import           Syntax
import           Text.Printf            (printf)
import           Util.Miscellaneous     (map1in4, show')
import Debug.Trace (trace )

type Generalizer = E.Sigma

class Generalization a g | g -> a where
  generalize :: [a] -> Generalizer -> Generalizer -> g -> g -> (g, Generalizer, Generalizer, [a])

instance Generalization S (G S) where
  generalize d gen1 gen2 (Invoke f as) (Invoke g bs) | f == g =
    map1in4 (Invoke f) $ generalize d gen1 gen2 as bs
  generalize _ _ _ x y =
    error (printf "Failed to generalize calls.\nAttempting to generalize\n%s\n%s\n" (show x) (show y))

instance Generalization S (Term S) where
  generalize vs s1 s2 (C m ms) (C n ns) | m == n =
    map1in4 (C m) $ generalize vs s1 s2 ms ns
  generalize vs s1 s2 (V x) (V y) | x == y = (V x, s1, s2, vs)
  generalize (v:vs) s1 s2 t1 t2 = (V v, (v, t1):s1, (v, t2):s2, vs)

instance Generalization S (f S) => Generalization S [f S] where
  generalize vs s1 s2 ns ms | length ns == length ms =
    map1in4 reverse $
    foldl (\(gs, gen1, gen2, vs) (t1, t2) ->
             map1in4 (:gs) $ generalize vs gen1 gen2 t1 t2
          )
          ([], s1, s2, vs)
          (zip ns ms)

generalizeGoals :: [S] -> [G S] -> [G S] -> ([G S], Generalizer, Generalizer, [S])
generalizeGoals s as bs | as `isRenaming` bs =
  let (Just subst) = inst as bs Map.empty in
  (bs, Map.toList subst, [], s)
generalizeGoals s as bs | length as == length bs =
  refine $ generalize s [] [] as bs
generalizeGoals _ as bs = error $ printf "Cannot generalize: different lengths of\nas: %s\nbs: %s\n" (show as) (show bs)

generalizeSplit :: [S] -> [G S] -> [G S] -> ([G S], [G S], Generalizer, Generalizer, [S])
generalizeSplit s gs hs =
    let (ok, notOk) = go gs hs in
    -- trace (printf "\n\nIn genearlizeSplit\n\ngs:\n%s\n\nhs:\n%s\n\nok:\n%s\n\nnotOk:\n%s\n\n" (show' gs) (show' hs) (show' ok) (show' notOk) ) $
    let (res, gen1, gen2, d) = generalizeGoals s gs ok in
    (res, notOk, gen1, gen2, d)
  where
    go gs hs | length gs == length hs = (hs, [])
    go (g : gs) (h : hs) | g `embed` h =
      let (ok, notOk) = go gs hs in
      (h : ok, notOk)
    go (g : gs) (h : hs) =
      let (ok, notOk) = go (g : gs) hs in
      (ok, h : notOk)
    go [] hs = ([], hs)

refine :: ([G S], Generalizer, Generalizer, [S]) ->  ([G S], Generalizer, Generalizer, [S])
refine msg@(g, s1, s2, d) =
  let similar1 = filter ((>1) . length) $ groupBy group s1 [] in
  let similar2 = filter ((>1) . length) $ groupBy group s2 [] in
  let sim1 = map (map fst) similar1 in
  let sim2 = map (map fst) similar2 in
  let toSwap = concatMap (\(x:xs) -> map (, V x) xs) (sim1 `intersect` sim2) in
  let newGoal = E.substitute toSwap g in
  let s2' = filter (\(x,_) -> x `notElem` map fst toSwap) s2 in
  let s1' = filter (\(x,_) -> x `notElem` map fst toSwap) s1 in
  (newGoal, s1', s2', d)
  where
    groupBy _ [] acc = acc
    groupBy p xs acc =
      let (similar, rest) = partition (p (head xs)) xs in
      assert (similar /= []) $ groupBy p rest (similar : acc)
    group x y = snd x == snd y
