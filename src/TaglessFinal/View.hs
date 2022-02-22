module TaglessFinal.View where

import TaglessFinal.Syntax
import TaglessFinal.Term

import Text.Printf ( printf )
import Control.Monad.State
import TaglessFinal.VarState

newtype View a = View { unView :: State VarState String }

instance Goal View where
  term x = View (return $ show x)
  unify x y = View $ do
    x <- unView x
    y <- unView y
    return $ printf "(%s == %s)" x y
  conj x y = View $ do
    x <- unView x
    y <- unView y
    return $ printf "(%s && %s)" x y
  disj x y = View $ do
    x <- unView x
    y <- unView y
    return $ printf "(%s || %s)" x y
  fresh f = View $ do
    v <- nextFreshVar
    let x = toFreshVar v
    body <- unView $ f x
    return $ printf "(fresh %s in %s)" x body

  lam f = View $ do
    v <- nextVar
    let x = toVar v
    body <- unView $ f (View $ return x)
    return $ printf "(\\%s -> %s)" x body

  -- var n = View $ return n

  fixP name f = View $ do
    body <- unView $ f (View $ return name)
    return $ printf "(fix \\%s -> %s)" name body

  app f arg = View $ do
    f <- unView f
    arg <- unView arg
    return $ printf "(app %s %s)" f arg

view :: View a -> String
view goal = evalState (unView goal) (VarState 0 0)