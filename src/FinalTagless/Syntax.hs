{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FinalTagless.Syntax where

import qualified Syntax

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

  safeConj :: goalRepr v -> [goalRepr v] -> goalRepr v
  safeConj x [] = x
  safeConj x (y:xs) = conj x y xs

  safeDisj :: goalRepr v -> [goalRepr v] -> goalRepr v
  safeDisj x [] = x
  safeDisj x (y:xs) = disj x y xs

  manyFresh :: [String] -> goalRepr v -> goalRepr v
  manyFresh names goal = foldr fresh goal names

instance (Show v) => Goal v S S where
  unif x y = S $ printf "%s === %s" (unS x) (unS y)
  conj x y xs = S $ printf "(%s)" (intercalate " /\\ " $ map unS (x : y : xs))
  disj x y xs = S $ printf "(%s)" (intercalate " \\/ " $ map unS (x : y : xs))
  fresh x g = S $ printf "(fresh %s %s)" x (unS g)
  call n args = S $ printf "%s(%s)" n (intercalate ", " $ map unS args)


newtype DeepTerm v = DeepTerm { unDeepTerm :: Syntax.Term v }

instance Term v DeepTerm where
  var = DeepTerm . Syntax.V
  con n args = DeepTerm $ Syntax.C n (map unDeepTerm args)

newtype Deep v = Deep { unDeep :: Syntax.G v }

instance Goal v DeepTerm Deep where
  unif x y = Deep $ (Syntax.:=:) (unDeepTerm x) (unDeepTerm y)
  conj x y xs = Deep $ Syntax.Conjunction (unDeep x) (unDeep y) (map unDeep xs)
  disj x y xs = Deep $ Syntax.Disjunction (unDeep x) (unDeep y) (map unDeep xs)
  fresh x g = Deep $ Syntax.Fresh x (unDeep g)
  call n args = Deep $ Syntax.Invoke n (map unDeepTerm args)

g1 :: Syntax.G [Char]
g1 =
  unDeep $
  disj
    (safeConj
      (unif (var "x") (con "nil" []))
      [unif (var "y") (var "z")])
    (manyFresh ["h", "t", "r"]
      (conj
        (unif (var "x") (con "cons" [var "h", var "t"]))
        (call "appendo" [var "t", var "y", var "r"])
        [unif (var "z") (con "cons" [var "h", var "r"])]))
    []

g2 = unDeep $ manyFresh ["x", "y"] (call "appendo" [var "x", var "y", con "nil" []])

g3 = unDeep $ manyFresh ["x", "y", "z"] $  conj (unif (var "x") (var "y")) (unif (var "y") (var "z")) [unif (var "x") (var "z")]

program3 = Syntax.Program [] g3

program1 = Syntax.Program [Syntax.Def "appendo" ["x", "y", "z"] g1] g2
