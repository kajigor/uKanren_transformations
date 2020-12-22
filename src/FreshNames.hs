{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FreshNames where

import Syntax
import Prelude hiding ((<))

newtype PolyFreshNames a = FreshNames { unFreshNames :: [a] }
                         deriving (Eq, Ord, Functor, Show)

type FreshNames = PolyFreshNames Int

getFreshName :: FreshNames -> (S, FreshNames)
getFreshName (FreshNames (h:t)) = (h, FreshNames t)

getNames :: Int -> FreshNames -> ([S], FreshNames)
getNames n (FreshNames ns) =
  let (xs, ys) = splitAt n ns in
  (xs, FreshNames ys)

defaultNames :: FreshNames
defaultNames = FreshNames [0..]

addName :: S -> FreshNames -> FreshNames
addName x (FreshNames d) = FreshNames (x:d)
