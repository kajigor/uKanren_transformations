{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FreshNames where

import           Prelude hiding ((<))
import           Data.Bifunctor

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

getNameStr :: FreshNames -> (String, FreshNames) 
getNameStr fn = first show $ getFreshName fn 

getNamesStr :: Int -> FreshNames -> ([String], FreshNames)
getNamesStr n fn = first (map show) $ getNames n fn 
