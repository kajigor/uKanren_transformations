{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module FreshNames where

import Prelude hiding ((<))

newtype PolyFreshNames a = FreshNames { unFreshNames :: a }
                         deriving (Show, Eq, Ord, Functor)

type FreshNames = PolyFreshNames Int

class FreshName a where
  getFreshName :: PolyFreshNames a -> (a, PolyFreshNames a)
  defaultNames :: PolyFreshNames a
  getNames :: Int -> PolyFreshNames a -> ([a], PolyFreshNames a)

instance FreshName Int where
  getFreshName (FreshNames x) = (x, FreshNames $ succ x)
  defaultNames = FreshNames 0
  getNames n (FreshNames x) =
    let (xs, h:_) = splitAt n $ enumFrom x
    in (xs, FreshNames h)
