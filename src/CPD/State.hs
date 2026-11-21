module CPD.State where 

import Control.Monad.State
import Syntax
import qualified Environment as Env
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Subst

data CPDState = CPDState
  { getSeen        :: Set.Set [G S]
  , getEnv         :: Env.Env
  , getLocalSteps  :: Int        -- ^ unfolding steps in the current local tree
  , getLocalBudget :: Maybe Int  -- ^ maximal unfolding steps per local tree
  , getAbstr       :: Map.Map ([G S], [G S]) [([G S], Subst.Subst S)]
    -- ^ (node goal, resultant goals) |-> the pieces global control split the
    --   resultant into, each with its generalizer (piece . gen = original)
  }

modifySeen :: (Set.Set [G S] -> Set.Set [G S]) -> State CPDState ()
modifySeen f =
  modify (\st -> st { getSeen = f (getSeen st) })

modifyEnv :: (Env.Env -> Env.Env) -> State CPDState ()
modifyEnv f =
  modify (\st -> st { getEnv = f (getEnv st) })

init :: Env.Env -> CPDState
init = initWithBudget Nothing

initWithBudget :: Maybe Int -> Env.Env -> CPDState
initWithBudget lb env = CPDState { getSeen = Set.empty, getEnv = env, getLocalSteps = 0, getLocalBudget = lb, getAbstr = Map.empty }