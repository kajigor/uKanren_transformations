{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module FinalTagless.Syntax where

import Text.Printf (printf)
import Data.List (intercalate)

class Term v repr where
  var :: v -> repr v
  con :: String -> [repr v] -> repr v

newtype S v = S { unS :: String }

instance Show v => Term v S where
  var v = S $ show v
  con n args = S $ printf "%s(%s)" n (intercalate ", " $ map unS args)

class Goal v termRepr goalRepr | goalRepr -> termRepr where
  unif :: termRepr v -> termRepr v -> goalRepr v
  conj :: goalRepr v -> goalRepr v -> [goalRepr v] -> goalRepr v
  disj :: goalRepr v -> goalRepr v -> [goalRepr v] -> goalRepr v
  fresh :: String -> goalRepr v -> goalRepr v
  call :: String -> [termRepr v] -> goalRepr v

instance (Show v) => Goal v S S where
  unif x y = S $ printf "%s === %s" (unS x) (unS y)
  conj x y xs = S $ printf "(%s)" (intercalate " /\\ " $ map unS (x : y : xs))
  disj x y xs = S $ printf "(%s)" (intercalate " \\/ " $ map unS (x : y : xs))
  fresh x g = S $ printf "(fresh %s %s)" x (unS g)
  call n args = S $ printf "%s(%s)" n (intercalate ", " $ map unS args)
