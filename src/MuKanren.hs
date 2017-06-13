{-
   Deeply embedded minikanren
-}

module MuKanren where
import Debug.Trace
import Control.Monad (foldM)
import Data
import State
import Stream


flatten :: State -> Term -> Term
flatten state (Free i) = Free i
flatten state (Var v)  =
  walkVar state (Var v)
  where
    walkVar s (Var v) =
      case getState s v of
        Var v' -> walk s (Var v')
        x -> x
    walkVar s x = x
flatten state (Ctor name ts) = Ctor name (map (flatten state) ts)

walk :: State -> Term -> Term
walk state term =
  walk' state t
  where
    t = flatten state term
    walk' state (Free i) =
      case lookup i (getSubst state) of
        Nothing -> Free i
        Just i' -> walk state i'
    walk' _ u = u

unify :: State -> Term -> Term -> Maybe State
unify state u v =
  unify' (walk state u) (walk state v)
  where
    unify' (Free u) (Free v) | u == v = Just state
    unify' u@(Free _) _ = Just (extSubst state u (flatten state v))
    unify' _ v@(Free _) = Just (extSubst state v (flatten state u))
    unify' (Ctor c1 ts1) (Ctor c2 ts2) | c1 == c2 =
      foldM (\s (u,v) -> unify s u v) state (zip ts1 ts2)
    unify' _ _ = Nothing

eval :: (String -> Def) -> State -> Goal -> Stream State
eval env state (Unify t1 t2) =
  case unify state t1 t2 of
    Nothing -> mzero
    Just st -> Mature st mzero
eval env state (Disj g1 g2) = eval env state g1 `mplus` eval env state g2
eval env state (Conj g1 g2) = eval env state g1 `bind` (\s -> eval env s g2)
eval env state (Fresh s g)  =
  eval env state' g
  where
    state' = newVar state s
eval env state (Zzz g) = Immature (eval env state g)
eval env state (Invoke f actualArgs) =
  eval env state' body
  where
    Def _ formalArgs body = env f
    state' = foldl bindVar state $ zip formalArgs (map (flatten state) actualArgs)

var = Var
(===) = Unify
(|||) = Disj
(&&&) = Conj
callFresh = Fresh
zzz = Zzz
nil = Ctor "Nil" []
cons h t = Ctor "Cons" [h,t]
