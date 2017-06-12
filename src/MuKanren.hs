{-
   Deeply embedded minikanren
-}

module MuKanren where
import Debug.Trace
import Data.List
import Control.Monad (foldM)

type Subst = [(Integer, Term)]

data State = State { subst :: Subst, state :: String -> Integer, index :: Integer}

data Term = Var String
          | Ctor String [Term]

data Goal = Unify Term Term
          | Disj Goal Goal
          | Conj Goal Goal
          | Fresh String Goal
          | Zzz Goal

data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion
              | Immature (Stream a)
              deriving Show

walk :: State -> Term ->  Term
walk s (Var v) =
  case lookup (state s v) (subst s) of
    Nothing -> Var v
    Just t  -> walk s t
walk _ u = u

extS :: State -> Term -> Term -> State
extS s (Var u) v =
  State { subst = (state s u,v) : subst s,
          state = state s,
          index = index s
        }

unify :: State -> Term -> Term -> Maybe State
unify s u v =
  -- do we need to add occurs check?
  unify' (walk s u) (walk s v)
  where
    unify' (Var u) (Var v) | u == v = Just s
    unify' u@(Var _) _ = Just (extS s u v)
    unify' _ v@(Var _) = Just (extS s v u)
    unify' (Ctor c1 ts1) (Ctor c2 ts2) | c1 == c2 =
      foldM (\s' (u,v) -> unify s' u v) s (zip ts1 ts2)
    unify' _ _ = Nothing

mzero :: Stream a
mzero = Empty

-- used for disjunctions, interleaves streams
mplus :: Stream a -> Stream a -> Stream a
mplus Empty _2 = _2
mplus (Mature h tl) _2 = Mature h (_2 `mplus` tl)
mplus (Immature _1) _2 = Immature (_2 `mplus` _1)

-- used for conjuctions
bind :: Stream  a -> (a -> Stream a) -> Stream a
bind Empty g = mzero
bind (Mature x xs) g = g x `mplus` bind xs g
bind (Immature _1) _2 = Immature (bind _1 _2)

newVar :: String -> State -> State
newVar s st =
  State { subst = subst st,
          state = \x -> if x == s then index st else state st x,
          index = index st + 1
        }

eval :: State -> Goal -> Stream State
eval state (Unify t1 t2) =
  case unify state t1 t2 of
    Nothing -> mzero
    Just st -> Mature st mzero
eval state (Disj g1 g2) = eval state g1 `mplus` eval state g2
eval state (Conj g1 g2) = eval state g1 `bind` (\state -> eval state g2)
eval state (Fresh s g) = eval state' g where
   state' = newVar s state
eval state (Zzz g) = Immature (eval state g)

var = Var
(===) = Unify
(|||) = Disj
(&&&) = Conj
callFresh = Fresh
zzz = Zzz
nil = Ctor "Nil" []
cons h t = Ctor "Cons" [h,t]

