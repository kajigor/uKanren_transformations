module TaglessFinal.EvalState where

import qualified TaglessFinal.VarState as VarState
import qualified TaglessFinal.Subst as Subst
import TaglessFinal.Term
import Control.Monad.State

data EvalState = EvalState
  { getVarState :: VarState.VarState
  , getSubst :: Subst.Subst Var
  }

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

