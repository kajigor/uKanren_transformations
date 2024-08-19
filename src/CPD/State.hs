module CPD.State where 

import Control.Monad.State
import Syntax
import qualified Environment as Env
import qualified Data.Set as Set

data CPDState = CPDState { getSeen :: Set.Set [G S], getEnv :: Env.Env }

modifySeen :: (Set.Set [G S] -> Set.Set [G S]) -> State CPDState ()
modifySeen f =
  modify (\st -> CPDState (f $ getSeen st) (getEnv st))

modifyEnv :: (Env.Env -> Env.Env) -> State CPDState ()
modifyEnv f =
  modify (\st -> CPDState (getSeen st) (f $ getEnv st))

init :: Env.Env -> CPDState 
init env = CPDState { getSeen = Set.empty, getEnv = env }