{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FreshNames where

import Syntax
import Prelude hiding ((<))
import Text.Printf (printf)

newtype PolyFreshNames a = FreshNames { unFreshNames :: [a] }
                         deriving (Ord, Functor)

type FreshNames = PolyFreshNames Int

-- Since there is no way to compare infinite lists, we'll only compare up to this depth
depth :: Int
depth = 20

instance Show a => Show (PolyFreshNames a) where
  show (FreshNames xs) =
    if length (take depth xs) >= depth
    then printf "[%s..]" (show $ head xs)
    else show xs

instance Eq a => Eq (PolyFreshNames a) where
  FreshNames xs == FreshNames ys =
    let xs' = take depth xs in
    let ys' = take depth ys in
    xs' == ys'



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
