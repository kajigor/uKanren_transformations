{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Embed where

import           Data.List       (find)
import           Util.Miscellaneous (subsequencesOfLength)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Maybe      (isJust)
import           Syntax
import           Debug.Trace

-- ordered subconjunctions of the proper length
subconjs :: [a] -> Int -> [[a]]
subconjs gs n = subsequencesOfLength n gs

-- | Is there an order-preserving injection of @gs@ into @hs@ whose pairs all
--   satisfy @p@?  Equivalent to @any (and . zipWith p gs) (subconjs hs (length gs))@,
--   but greedy and O(|gs| * |hs|) instead of enumerating C(|hs|, |gs|)
--   subsequences: matching each element to the earliest admissible one is
--   optimal, since it leaves the longest suffix for the rest.
subseqMatch :: (a -> b -> Bool) -> [a] -> [b] -> Bool
subseqMatch _ [] _ = True
subseqMatch _ _ [] = False
subseqMatch p gs@(g : gs') (h : hs)
  | p g h     = subseqMatch p gs' hs
  | otherwise = subseqMatch p gs hs

class Ground a where
  isGround :: a -> Bool

instance Ground a => Ground [a] where
  isGround = all isGround

instance Ground (Term a) where
  isGround (V _) = False
  isGround (C _ args) = isGround args

instance Ground (G a) where
  isGround (Conjunction x y gs) = all isGround (x : y : gs)
  isGround (Disjunction x y gs) = all isGround (x : y : gs)
  isGround (u :=: v) = isGround u && isGround v
  isGround (Invoke _ args) = isGround args

class AlwaysEmbeddable a => Homeo a where
  couple :: a -> a -> Bool
  diving :: a -> a -> Bool
  homeo  :: a -> a -> Bool
  homeo x y = couple x y || diving x y

instance Homeo (Term a) where
  couple (C n as) (C m bs) | n == m && length as == length bs =
    all (uncurry homeo) $ zip as bs
  couple _ _ = False

  diving v (C _ as) = any (homeo v) as
  diving _ _ = False

  homeo (V _) (V _) = True
  -- homeo x y | isSucc x && isSucc y = homeo (predec x) (predec y)
  --   where
  --     isSucc (C s [n]) = map toLower s == "s"
  --     isSucc _ = False
  --
  --     predec c@(C _ [a]) | isSucc c = a
  --     predec c = error $ printf "Failed to get predecessor"

  homeo x y = couple x y || diving x y

instance Show a => Homeo (G a) where
  couple goal@(Invoke f as) (Invoke g bs) | isAlwaysEmbeddable goal || f == g && length as == length bs =
    all (uncurry homeo) $ zip as bs
  couple _ _ = False

  diving _ _ = False

instance Show a => Homeo [G a] where
  couple = undefined
  diving = undefined

  homeo gs hs =
    subseqMatch homeo gs hs
  -- couple gs hs | length gs == length hs =
  --   all (uncurry homeo) $ zip gs hs
  -- couple _ _ = False
  --
  -- diving gs (h:hs) = homeo gs hs
  -- diving _ _ = False

class (Eq b, Show b, Show a) => Instance a b | b -> a where
  inst :: b -> b -> Map.Map a (Term a) -> Maybe (Map.Map a (Term a))

  isInst :: b -> b -> Bool
  isInst x y =
    isJust $ inst x y Map.empty

  isStrictInst :: b -> b -> Bool
  isStrictInst x y = isInst x y && not (isInst y x)

  isVariant :: b -> b -> Bool
  isVariant x y = x == y || isInst x y && isInst y x

  isRenaming :: b -> b -> Bool --tests
  isRenaming x y =
    x == y || maybe False (all (\e -> case e of V _ -> True; _ -> False ) . Map.elems) (inst x y Map.empty)

  instanceCheck :: Foldable t => b -> t b -> Bool
  instanceCheck g = any (`isInst` g)
  -- instanceCheck g = any (isInst g)

  variantCheck :: Foldable t => b -> t b -> Bool
  variantCheck g = any (isVariant g)

  findVariant :: Foldable t => b -> t b -> Maybe b
  findVariant g = find (isVariant g)

  findInstance :: Foldable t => b -> t b -> Maybe b
  findInstance g = find (`isInst` g)

instance (Eq a, Ord a, Show a) => Instance a (Term a) where
  inst (V v) u subst =
    case Map.lookup v subst of
      Just w | u == w -> Just subst
      Just w -> Nothing
      Nothing ->
        Just $ Map.insert v u subst
  inst (C n as) (C m bs) subst | n == m = instList as bs subst
  inst _ _ _ = Nothing

instance (Eq a, Ord a, Show a) => Instance a (G a) where
  inst (Invoke f as) (Invoke g bs) subst | f == g = instList as bs subst
  inst _ _ _ = Nothing


instance (Eq a, Ord a, Show a) => Instance a [G a] where
  inst = instList

-- | Pairwise 'inst' of two lists of equal length, stopping at the first
--   mismatch.  (Comparing the lengths first and folding over the whole zip
--   made the instance check against every seen conjunction cost O(length)
--   even when the first goals already differ: 65% of ConsPD's time on
--   contains@vanilla.)
instList :: Instance a b => [b] -> [b] -> Map.Map a (Term a) -> Maybe (Map.Map a (Term a))
instList [] [] subst = Just subst
instList (a : as) (b : bs) subst = inst a b subst >>= instList as bs
instList _ _ _ = Nothing

class AlwaysEmbeddable a where
  isAlwaysEmbeddable :: a -> Bool

instance AlwaysEmbeddable (G a) where
  isAlwaysEmbeddable (Invoke f _) = f `elem` [] --[] -- ["leo", "gto"]
  isAlwaysEmbeddable _ = False

instance AlwaysEmbeddable [G a] where
  isAlwaysEmbeddable = null

instance AlwaysEmbeddable (Term a) where
  isAlwaysEmbeddable = const True

-- Strict homeomorphic embedding. Explore: use a variants check instead of the instance check.
class (Homeo b, Instance a b, Eq b, Show a) => Embed a b | b -> a where
  embed :: b -> b -> Bool
  embed g h = -- trace (show g ++ show h ++ show (homeo g h && not (isStrictInst h g))) $ 
    isAlwaysEmbeddable g || g == h || homeo g h && not (isStrictInst h g)

instance (Ord a, Eq a, Show a) => Embed a (G a)
instance (Ord a, Eq a, Show a) => Embed a [G a] where
  embed gs hs = subseqMatch embed gs hs

-- | @instanceCheckSet g seen == instanceCheck g seen@ for a set of
--   conjunctions.  An instance has the same relations in the same order, and
--   the derived order of @[G a]@ puts all conjunctions that start with a call of
--   @f@ into one contiguous range of the set, so only that range is scanned.
--   (Scanning all of @seen@ at every unfolding step was 56% of ConsPD's time
--   on contains@vanilla, quadratic in the number of processed conjunctions.)
instanceCheckSet :: (Ord a, Show a) => [G a] -> Set.Set [G a] -> Bool
instanceCheckSet g@(Invoke f _ : _) seen =
  let probe = Invoke f []          -- the least call of f in the derived order
      from = Set.dropWhileAntitone (\x -> case x of { [] -> True; (h : _) -> h < probe }) seen
      range = Set.takeWhileAntitone (\x -> case x of { (Invoke f' _ : _) -> f' == f; _ -> False }) from
  in any (`isInst` g) range
instanceCheckSet g seen = instanceCheck g seen
