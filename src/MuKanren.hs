{-
   Deeply embedded minikanren
-}

module MuKanren where
import Debug.Trace
import Data.List
import Control.Monad (foldM)

type Subst = [(Integer, Term)]

data State = State { subst :: Subst, state :: String -> Term, index :: Integer}

data Def = Def { name :: String, args :: [String], body :: Goal }

data Spec = Spec { defs :: [Def], goal :: Goal }

data Term = Var String
          | Ctor String [Term]
          | Free Integer

data Goal = Unify Term Term
          | Disj Goal Goal
          | Conj Goal Goal
          | Fresh String Goal
          | Zzz Goal
          | Invoke String [Term]

data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion
              | Immature (Stream a)

showSubst s = "[" ++ intercalate ", " (map (\(i,t) -> "x." ++ show i ++ " -> " ++ show t) s) ++  "]"

instance Show State where
  show (State subst _ index) = "St " ++ showSubst subst ++ " " ++ show index

instance Show Term where
  show (Var v) = v
  show (Free i) = "x." ++ show i
  show (Ctor ctor ts) = ctor ++ "(" ++ showList ts ")"

instance Show Goal where
  show (Unify l r) = "(" ++ show l ++ " === " ++ show r ++ ")"
  show (Conj l r)  = "(" ++ show l ++ " &&& " ++ show r ++ ")"
  show (Disj l r)  = "(" ++ show l ++ " ||| " ++ show r ++ ")"
  show (Fresh v g) = "Fresh " ++ v ++ " " ++ show g
  show (Invoke n args) = "Invoke " ++ n ++ " with (" ++ show args ++ ")"
  show (Zzz g) = "Zzz " ++ show g

instance Show a => Show (Stream a) where
  show Empty = "[]"
  show (Mature a s) = show a ++ " : " ++ show s
  show (Immature s) = "Immature " ++ show s

walkVar s (Var v) =
  case state s v of
    Var v' -> walk s (Var v')
    x -> x
walkVar s x = x

spreadLogic s (Var v) = walkVar s (Var v)
spreadLogic s (Free i) = Free i
spreadLogic s (Ctor name ts) = Ctor name (map (spreadLogic s) ts)

walk :: State -> Term ->  Term
walk s (Var v) =
--  trace ("walking " ++ show v ++ " (" ++ show (state s v) ++ ")") $
  case state s v of
    Free v' -> case lookup v' (subst s) of
                 Nothing -> Free v'
                 Just t  -> walk s t
    v'@(Var v'') -> error "var to var"
    v'@(Ctor _ _) -> v'
walk s (Free i) =
  case lookup i (subst s) of
    Nothing -> Free i
    Just i' -> walk s i'
walk _ u = u

extS :: State -> Term -> Term -> State
extS s (Free u) v =
  State { subst = (u,v) : subst s,
          state = state s,
          index = index s
        }

unify :: State -> Term -> Term -> Maybe State
unify s u v =
  let u' = walk s u
      v' = walk s v
  in
--     trace ("unifying " ++ show u ++ "(" ++ show u' ++ ")" ++ " and " ++ show v ++ "(" ++ show v' ++ ")" ++ " in state\n" ++ show s) $
  -- do we need to add occurs check?
      unify' (walk s u) (walk s v)
        where
          unify' (Free u) (Free v) | u == v = Just s
          unify' u@(Free _) _ =
--                                trace ("yep " ++ show (extS s u v)) $
                                Just (extS s u (spreadLogic s v))
          unify' _ v@(Free _) = Just (extS s v (spreadLogic s u))
          unify' l@(Ctor c1 ts1) r@(Ctor c2 ts2) | c1 == c2 =
--            trace ("unifying " ++ show l ++ " and " ++ show r) $
            foldM (\s' (u,v) -> unify s' u v) s (zip ts1 ts2)
          unify' _ _ = Nothing

mzero :: Stream a
mzero = Empty

emptyState :: State
emptyState = State { subst = [], state = undefined, index = 0 }

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

extend :: State -> String -> Term -> (String -> Term)
extend st var value =
  \x ->
--        trace ("extending state with " ++ var ++ " -> " ++ show value) $
        if x == var then value else state st x

newVar :: String -> State -> State
newVar s st =
  State { subst = subst st
        , state = extend st s (Free $ index st)
        , index = index st + 1
        }

eval :: (String -> Def) -> State -> Goal -> Stream State
eval env state (Unify t1 t2) =
  case unify state t1 t2 of
    Nothing -> mzero
    Just st -> Mature st mzero
eval env state (Disj g1 g2) = eval env state g1 `mplus` eval env state g2
eval env state (Conj g1 g2) = eval env state g1 `bind` (\state -> eval env state g2)
eval env state (Fresh s g) = eval env state' g where
  state' = newVar s state
eval env state (Zzz g) = Immature (eval env state g)
eval env state (Invoke f actual_args) =
  let Def _ formal_args body = env f in
  let state' = foldl bindVar state $ zip formal_args (map (walkVar state) actual_args)  in
--  trace ("\nInvoke " ++ f ++ "\nin state " ++ show state' ++ "\nargs: " ++ show (zip formal_args actual_args) ++ "\n")  $
  eval env state' body

bindVar st (var, value) =
  State { subst = subst st
        , state = extend st var value
        , index = index st
        }

var = Var
(===) = Unify
(|||) = Disj
(&&&) = Conj
callFresh = Fresh
zzz = Zzz
nil = Ctor "Nil" []
cons h t = Ctor "Cons" [h,t]

