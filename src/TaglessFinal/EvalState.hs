module TaglessFinal.EvalState where

import qualified TaglessFinal.VarState as VarState
import qualified TaglessFinal.Subst as Subst
import TaglessFinal.Term
import Control.Monad.State

data EvalState = EvalState
  { getVarState :: VarState.VarState
  , getSubst :: Subst.Subst Var
  }

getSubstM :: State EvalState (Subst.Subst Var)
getSubstM = do
  state <- get
  return (getSubst state)

nextFreshVar :: State EvalState Int
nextFreshVar = do
  state <- get
  let varState = getVarState state
  let subst = getSubst state
  let x = VarState.getFreshVar varState
  let newState = EvalState
        { getVarState = VarState.VarState
            { VarState.getVar = VarState.getVar varState
            , VarState.getFreshVar = x + 1
            }
        , getSubst = subst
        }
  return x

nextVar :: State EvalState Int
nextVar = do
  state <- get
  let varState = getVarState state
  let subst = getSubst state
  let x = VarState.getVar varState
  let newState = EvalState
        { getVarState = VarState.VarState
            { VarState.getVar = x + 1
            , VarState.getFreshVar = VarState.getFreshVar varState
            }
        , getSubst = subst
        }
  return x

updateSubst :: (Subst.Subst Var -> Subst.Subst Var) -> State EvalState (Subst.Subst Var)
updateSubst f = do
  state <- get
  let varState = getVarState state
  let newSubst = f (getSubst state)
  put $ EvalState { getVarState = varState, getSubst = newSubst }
  return newSubst