{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE FlexibleInstances #-}
module TaglessFinal.Syntax where

import Text.Printf ( printf )
import Control.Monad.State
import TaglessFinal.VarState

type Name = String
type Var = String

data Term = Var Var  | Con Name [Term]
            deriving (Show, Eq)

class Goal repr where
  unify :: Term -> Term -> repr
  conj :: repr -> repr -> repr
  disj :: repr -> repr -> repr
  fresh :: (Var -> repr) -> repr

  lam :: (Term -> repr) -> repr
  app :: repr -> Term -> repr
  fixP :: String -> (repr -> repr) -> repr
  var :: String -> repr

newtype View = View { unView :: State VarState String }

instance Goal View where
  unify x y = View $ return $ printf "(%s == %s)" (show x) (show y)
  conj x y = View $ do
    x' <- unView x
    y' <- unView y
    return $ printf "(%s && %s)" x' y'
  disj x y = View $ do
    x' <- unView x
    y' <- unView y
    return $ printf "(%s || %s)" x' y'
  fresh f = View $ do
    v <- nextFreshVar
    let x = printf "x.%s" (show v) :: String
    body <- unView $ f x
    return $ printf "(fresh %s in %s)" x body

  lam f = View $ do
    v <- nextVar
    let x = printf "v.%s" (show v) :: String
    body <- unView $ f (Var x)
    return $ printf "(\\%s -> %s)" x body

  var n = View $ return n

  fixP name f = View $ do
    body <- unView $ f (var name)
    return $ printf "(fix \\%s -> %s)" name body

  app f arg = View $ do
    application <- unView f
    return $ printf "(app %s %s)" application (show arg)


view goal = evalState (unView goal) (VarState 0 0)

appendo :: Goal repr => repr
appendo =
  lam $ \x -> lam $ \y -> lam $ \z ->
    fixP "appendo" ( \self ->
        disj (conj (unify x (Con "Nil" []))
                  (unify y z))
            (fresh $ \h -> fresh $ \t -> fresh $ \r ->
              (conj (unify x (Con "Cons" [Var h, Var t]))
                    (conj (app (app (app self (Var t)) y) (Var r))
                          -- appendo (Var t) y (Var r)
                          (unify z (Con "Cons" [Var h, Var r]))))
            )
    )