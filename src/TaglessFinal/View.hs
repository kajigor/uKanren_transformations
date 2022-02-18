module TaglessFinal.View where

import TaglessFinal.Syntax
import TaglessFinal.Term

import Text.Printf ( printf )
import Control.Monad.State
import TaglessFinal.VarState

newtype View a = View { unView :: State VarState String }

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
    let x = toFreshVar v
    body <- unView $ f x
    return $ printf "(fresh %s in %s)" x body

  lam f = View $ do
    v <- nextVar
    let x = toVar v
    body <- unView $ f (Var x)
    return $ printf "(\\%s -> %s)" x body

  -- var n = View $ return n

  fixP name f = View $ do
    body <- unView $ f (View $ return name)
    return $ printf "(fix \\%s -> %s)" name body

  app f arg = View $ do
    application <- unView f
    return $ printf "(app %s %s)" application (show arg)

view :: View a -> String
view goal = evalState (unView goal) (VarState 0 0)