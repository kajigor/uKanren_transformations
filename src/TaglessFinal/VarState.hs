module TaglessFinal.VarState where

import Control.Monad.State

data VarState = VarState
  { getVar :: Int
  , getFreshVar :: Int
  }

nextFreshVar :: State VarState Int
nextFreshVar = do
  state <- get
  let x = getFreshVar state
  put $ VarState { getVar = getVar state, getFreshVar = x + 1}
  return x

nextVar :: State VarState Int
nextVar = do
  state <- get
  let x = getVar state
  put $ VarState { getVar = x + 1, getFreshVar = getFreshVar state }
  return x
