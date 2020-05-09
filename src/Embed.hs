{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Embed where

import Syntax
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.List (subsequences, find)

-- ordered subconjunctions of the proper length
subconjs :: [a] -> Int -> [[a]]
subconjs gs n = filter (\x -> n == length x) $ subsequences gs

class Ground a where
  isGround :: a -> Bool

instance Ground a => Ground [a] where
  isGround = all isGround

instance Ground (Term a) where
  isGround (V _) = False
  isGround (C _ args) = isGround args

instance Ground (G a) where
  isGround (g :/\: h) = isGround g && isGround h
  isGround (g :\/: h) = isGround g && isGround h
  isGround (u :=:  v) = isGround u && isGround v
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
    any (all (uncurry homeo) . zip gs) (subconjs hs (length gs))
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

  isRenaming :: b -> b -> Bool
  isRenaming x y =
    x == y || maybe False (all (\e -> case e of V _ -> True; _ -> False ) . Map.elems) (inst x y Map.empty)

  instanceCheck :: Foldable t => b -> t b -> Bool
  instanceCheck g = any (`isInst` g)
  -- instanceCheck g = any (isInst g)

  variantCheck :: Foldable t => b -> t b -> Bool
  variantCheck g = any (isVariant g)

  findVariant :: Foldable t => b -> t b -> Maybe b
  findVariant g = find (isVariant g)

instance (Eq a, Ord a, Show a) => Instance a (Term a) where
  inst (V v) u subst =
    case Map.lookup v subst of
      Just w | u == w -> Just subst
      Just w -> Nothing
      Nothing ->
        Just $ Map.insert v u subst
  inst (C n as) (C m bs) subst | n == m && length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing

instance (Eq a, Ord a, Show a) => Instance a (G a) where
  inst (Invoke f as) (Invoke g bs) subst | f == g && length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing


instance (Eq a, Ord a, Show a) => Instance a [G a] where
  inst as bs subst | length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing

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
  embed g h =
    isAlwaysEmbeddable g || g == h || homeo g h && not (isStrictInst h g)

instance (Ord a, Eq a, Show a) => Embed a (G a)
instance (Ord a, Eq a, Show a) => Embed a [G a] where
  embed gs hs =
    let subs = subconjs hs (length gs) in
    any (and . zipWith embed gs) subs