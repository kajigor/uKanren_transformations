{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OfflinePD.Generalization where

import           Control.Monad.State
import           Control.Exception.Base
import           Generalization       hiding (refine, generalizeGoals)
import           BTA.InvokeAnnotation
import           Data.List              hiding (group, groupBy)
import           Embed
import qualified Data.Map             as Map
import           Syntax (Term(..), S)
import qualified FreshNames           as FN
import qualified Subst
import           Text.Printf            (printf)
import           Util.Miscellaneous     (map1in3)
import           Descend


instance Subst.ApplySubst (AnnG Term S) where
  substitute s (Invoke name as ann) = Invoke name (map (Subst.substitute s) as) ann
  substitute _ g = error $ printf "We have only planned to substitute into calls, and you are trying to substitute into:\n%s" (show g)

instance Subst.ApplySubst [AnnG Term S] where
  substitute = map . Subst.substitute
  
instance Subst.ApplySubst [Descend (AnnG Term S)] where
  substitute s =
    map $ \(Descend g ancs) -> Descend (Subst.substitute s g) ancs
    
instance Ground (AnnG Term a) where
  isGround (Conjunction x y gs) = all isGround (x : y : gs)
  isGround (Disjunction x y gs) = all isGround (x : y : gs)
  isGround (u :=: v) = isGround u && isGround v
  isGround (Invoke _ args _ ) = isGround args
  
instance Show a => Homeo (AnnG Term a) where
  couple goal@(Invoke f as _) (Invoke g bs _) | isAlwaysEmbeddable goal || f == g && length as == length bs =
    all (uncurry homeo) $ zip as bs
  couple _ _ = False
  diving _ _ = False

instance Show a => Homeo [AnnG Term a] where
  couple = undefined
  diving = undefined

  homeo gs hs =
    any (all (uncurry homeo) . zip gs) (subconjs hs (length gs))


instance (Eq a, Ord a, Show a) => Instance a (AnnG Term a) where
  inst (Invoke f as _) (Invoke g bs _) subst | f == g && length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing


instance (Eq a, Ord a, Show a) => Instance a [AnnG Term a] where
  inst as bs subst | length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing
  

instance AlwaysEmbeddable (AnnG Term a) where
  isAlwaysEmbeddable (Invoke f _ _) = f `elem` [] --[] -- ["leo", "gto"]
  isAlwaysEmbeddable _ = False

instance AlwaysEmbeddable [AnnG Term a] where
  isAlwaysEmbeddable = null
  
instance (Ord a, Eq a, Show a) => Embed a (AnnG Term a)
instance (Ord a, Eq a, Show a) => Embed a [AnnG Term a] where
  embed gs hs =
    let subs = subconjs hs (length gs) in
    any (and . zipWith embed gs) subs
    
instance Generalization S (AnnG Term S) where
  generalize gen1 gen2 (Invoke f as ann) (Invoke g bs _) | f == g =
    map1in3 (\x -> Invoke f x ann) <$> generalize gen1 gen2 as bs
  generalize _ _ x y =
    error (printf "Failed to generalize calls.\nAttempting to generalize\n%s\n%s\n" (show x) (show y))
    

generalizeGoals :: FN.FreshNames -> [AnnG Term S] -> [AnnG Term S] -> ([AnnG Term S], Generalizer, Generalizer, FN.FreshNames)
generalizeGoals s as bs | as `isRenaming` bs =
  let (Just subst) = inst as bs Map.empty in
  (bs, Subst.Subst subst, Subst.empty, s)
generalizeGoals s as bs | length as == length bs =
  let ((g, gen1, gen2), d) = runState (generalize Subst.empty Subst.empty as bs) s in
  refine (g, gen1, gen2, d)
generalizeGoals _ as bs = error $ printf "Cannot generalize: different lengths of\nas: %s\nbs: %s\n" (show as) (show bs)


refine :: ([AnnG Term S], Generalizer, Generalizer, FN.FreshNames) ->  ([AnnG Term S], Generalizer, Generalizer, FN.FreshNames)
refine msg@(g, Subst.Subst s1, Subst.Subst s2, d) =
    let similar1 = filter ((>1) . length) $ groupBy group (Map.toList s1) [] in
    let similar2 = filter ((>1) . length) $ groupBy group (Map.toList s2) [] in
    let sim1 = map (map fst) similar1 in
    let sim2 = map (map fst) similar2 in
    let toSwap = concatMap (\(x:xs) -> map (, V x) xs) (sim1 `intersect` sim2) in
    let newGoal = Subst.substitute (Subst.Subst $ Map.fromList toSwap) g in
    let s2' = filter (\(x,_) -> x `notElem` map fst toSwap) (Map.toList s2) in
    let s1' = filter (\(x,_) -> x `notElem` map fst toSwap) (Map.toList s1) in
    (newGoal, Subst.Subst $ Map.fromList s1', Subst.Subst $ Map.fromList s2', d)
  where
    groupBy _ [] acc = acc
    groupBy p xs acc =
      let (similar, rest) = partition (p (head xs)) xs in
      assert (similar /= []) $ groupBy p rest (similar : acc)
    group x y = snd x == snd y
