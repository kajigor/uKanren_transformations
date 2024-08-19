module ConsPD.State where 

import Control.Monad.State
import Syntax
import qualified Environment as Env
import qualified Data.Set as Set

data ConsPDState = ConsPDState {getSeen :: Set.Set [G S], getFailed :: Set.Set [G S], getEnv :: Env.Env}

modifySeen :: (Set.Set [G S] -> Set.Set [G S]) -> State ConsPDState ()
modifySeen f =
  modify (\st -> ConsPDState (f $ getSeen st) (getFailed st) (getEnv st))

modifyFailed :: (Set.Set [G S] -> Set.Set [G S]) -> State ConsPDState ()
modifyFailed f =
  modify (\st -> ConsPDState (getSeen st) (f $ getFailed st) (getEnv st))

modifyEnv :: (Env.Env -> Env.Env) -> State ConsPDState ()
modifyEnv f =
  modify (\st -> ConsPDState (getSeen st) (getFailed st) (f $ getEnv st))

init :: Env.Env -> ConsPDState 
init env = ConsPDState { getSeen = Set.empty, getFailed = Set.empty, getEnv = env }