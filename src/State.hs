module State where
import Data
import Data.Maybe (fromMaybe)

emptyState :: State
emptyState = State { getSubst = [], getState = undefined, index = 0, vars = [] }

extState :: State -> String -> Term -> (String -> Term)
extState state var value =
  \x -> if x == var then value else getState state x

extSubst :: State -> Term -> Term -> State
extSubst state (Free u) v =
  State { getSubst = (u,v) : getSubst state
        , getState  = getState state
        , index = index state
        , vars = vars state
        }

newVar :: State -> String -> State
newVar state var =
  State { getSubst = getSubst state
        , getState = extState state var (Free $ index state)
        , index = index state + 1
        , vars = var : vars state
        }

bindVar :: State -> (String, Term) -> State
bindVar state (var, value) =
  State { getSubst = getSubst state
        , getState = extState state var value
        , index = index state
        , vars = var : vars state
        }

subst state (Ctor n ts) = Ctor n $ map (subst state) ts
subst state (Var n)     = getState state n
subst state t           = t

substG state (Unify t1 t2) = Unify (subst  state t1) (subst  state t2)
substG state (Disj  xs   ) = Disj  (map (substG state) xs)
substG state (Conj  xs   ) = Conj  (map (substG state) xs)
substG state (Fresh x g)   =
  let state' = bindVar state (x, Var x)
  in Fresh x (substG state' g)
substG state (Invoke f as) = Invoke f (map (subst state) as)
substG state (Zzz a) = Zzz (substG state a)


applyState state goal =
  case goal of
    Unify l r -> Unify (apply l state) (apply r state)
    Disj  xs  -> Disj  (map (applyState state) xs)
    Conj  xs  -> Conj  (map (applyState state) xs)
    Fresh v g -> Fresh v (applyState state g)
    Zzz     g -> Zzz (applyState state g)
    Invoke n args -> Invoke n (map (`apply` state) args)
  where
    apply v state =
      case v of
        Free i -> fromMaybe (Free i) (lookup i (getSubst state))
        Var  v -> if v `elem` vars state then getState state v else Var v
        Ctor n ts -> Ctor n (map (`apply` state) ts)
