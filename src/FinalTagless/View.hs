{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FinalTagless.View where

import FinalTagless.GoalSyntax

import Text.Printf (printf)
import Data.List (intercalate)

newtype View v = View { unView :: String }

instance (Show v) => Goal v View where
  unif x y = View $ printf "%s === %s" (show x) (show y)
  conj x y xs = View $ printf "(%s)" (intercalate " /\\ " $ map unView (x : y : xs))
  disj x y xs = View $ printf "(%s)" (intercalate " \\/ " $ map unView (x : y : xs))
  fresh x g = View $ printf "(fresh %s %s)" (show x) (unView g)
  call n args = View $ printf "%s(%s)" n (intercalate ", " (map show args))

g1Viewed = unView g1
g2Viewed = unView g2
g3Viewed = unView g3
