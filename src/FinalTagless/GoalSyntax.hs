{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FinalTagless.GoalSyntax where

import qualified Syntax


class Goal v goalRepr where
  unif :: Syntax.Term v -> Syntax.Term v -> goalRepr v
  conj :: goalRepr v -> goalRepr v -> [goalRepr v] -> goalRepr v
  disj :: goalRepr v -> goalRepr v -> [goalRepr v] -> goalRepr v
  fresh :: v -> goalRepr v -> goalRepr v
  call :: String -> [Syntax.Term v] -> goalRepr v

  safeConj :: goalRepr v -> [goalRepr v] -> goalRepr v
  safeConj x [] = x
  safeConj x (y:xs) = conj x y xs

  safeDisj :: goalRepr v -> [goalRepr v] -> goalRepr v
  safeDisj x [] = x
  safeDisj x (y:xs) = disj x y xs

  manyFresh :: [v] -> goalRepr v -> goalRepr v
  manyFresh names goal = foldr fresh goal names

var = Syntax.V
con = Syntax.C

g1 :: Goal Syntax.X goalRepr => goalRepr Syntax.X
g1 =
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

g2 = manyFresh ["x", "y"] (call "appendo" [var "x", var "y", con "nil" []])

g3 = manyFresh ["x", "y", "z"] $  conj (unif (var "x") (var "y")) (unif (var "y") (var "z")) [unif (var "x") (var "z")]

g4 = (unif (con "nil" []) (con "cons" [var "x", con "nil" []]))
