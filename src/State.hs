module State where
import Data

emptyState :: State
emptyState = State { getSubst = [], getState = undefined, index = 0 }

extState :: State -> String -> Term -> (String -> Term)
extState state var value =
  \x -> if x == var then value else getState state x

extSubst :: State -> Term -> Term -> State
extSubst state (Free u) v =
  State { getSubst = (u,v) : getSubst state
        , getState  = getState state
        , index = index state
        }

newVar :: State -> String -> State
newVar state var =
  State { getSubst = getSubst state
        , getState = extState state var (Free $ index state)
        , index = index state + 1
        }

bindVar :: State -> (String, Term) -> State
bindVar state (var, value) =
  State { getSubst = getSubst state
        , getState = extState state var value
        , index = index state
        }

