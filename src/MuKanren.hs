{-
   Implementation of MicroKanren as in the paper
   "µKanren: A Minimal Functional Core for Relational Programming"
   by Jason Hemann and Daniel P. Friedman
-}

module MuKanren where
import Debug.Trace
import Data.List

type Var = Int
-- for now, only integer atoms are allowed
data Term = Var Var | Pair Term Term | Atom Int | Nil | R Var deriving Eq

instance Show Term where
  show (Var  v)   = "x." ++ show v
  show (Pair v u) = "(" ++ show v ++ ", " ++ show u ++ ")"
  show (Atom i)   = show i
  show Nil        = "[]"
  show (R v)      = "_." ++ show v

data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion (who would have known)
              | Immature (Stream a)
              deriving Show


type Name = String
data AST = Uni Term Term
         | Conj AST AST
         | Disj AST AST
         | Fresh (Term -> AST)
         | Fun Name AST
         | Call AST [Term]
         | Zzz AST

instance Show AST where
  show (Uni  x y) = "(" ++ show x ++ " === " ++ show y ++ ")"
  show (Conj x y) = "(" ++ show x ++ " &&& " ++ show y ++ ")"
  show (Disj x y) = "(" ++ show x ++ " ||| " ++ show y ++ ")"
  show (Fun  n b) = "(" ++ n ++ "(" ++ show b ++ ")" ++ ")"
  show (Fresh  f) = "(" ++ "fresh" ++ ")"
  show (Call (Fun n _) arg) = "(" ++ "call " ++ n ++ " with [" ++ concatMap (\x -> show x ++ "; ") arg ++ "]" ++ ")"
  show (Zzz a) = "zzz " ++ show a ++ ")"

show' _ c | c > 20 = ""
show' (Uni  x y) c = show x ++ " === " ++ show y
show' (Conj x y) c = show' x c ++ " &&& " ++ show' y c
show' (Disj x y) c = show' x c ++ " ||| " ++ show' y c
show' (Fun  n b) c = n ++ "(" ++ show' b c ++ ")"
show' (Fresh  f) c = "fresh " ++ show' (f (var c)) (c + 1)
show' (Call x arg) c = "call " ++ show' x c ++ " with [" ++ concatMap (\x -> show x ++ "; ") arg ++ "]"
show' (Zzz a) c = "zzz " ++ show' a c

-- Combinators to write programs with
var = Var
at = Atom

(===) = Uni
(|||) = Disj
(&&&) = Conj
call_fresh = Fresh
fun = Fun
zzz = Zzz
nil = Nil
pair = Pair
call = Call
list xs = foldr pair nil xs

seq2 f [x]    = x
seq2 f (x:xs) = x `f` seq2 f xs

conj = seq2 (&&&)
disj = seq2 (|||)

conde ds = disj (map conj ds)

empty_subst = []
empty_state = (empty_subst, 0 :: Int)

mzero = Empty

-- used for disjunctions, interleaves streams
mplus Empty _2 = _2
mplus (Mature h tl) _2 = Mature h (_2 `mplus` tl)
mplus (Immature _1) _2 = Immature (_2 `mplus` _1)

-- used for conjuctions
bind Empty g = mzero
bind (Mature x xs) g = g x `mplus` bind xs g
bind (Immature _1) _2 = Immature (bind _1 _2)

walk (Var v) s =
  case lookup v s of
    Nothing -> Var v
    Just t  -> walk t s
walk u _ = u

ext_s u v s =
  (u, v) : s
  {- let v' = walk v s
      occurs_check (Var x) (Var v) = x == v
      occurs_check x (Pair v u) = occurs_check x v s || occurs_check x u s
      occurs_check _ _ = False
  in
  case occurs_check u v' s of
    False -> Just ((u, v) : s)
    True  -> Nothing -}

unify u v s =
  -- do we need to add occurs check?
  unify' (walk u s) (walk v s)
  where
    unify' (Var u) (Var v) | u == v = Just s
    unify' (Var u) _ = Just (ext_s u v s)
    unify' _ (Var v) = Just (ext_s v u s)
    unify' (Pair u u') (Pair v v') =
      case unify u v s of
        Nothing -> Nothing
        Just s' -> unify u' v' s'
    unify' (Atom u) (Atom v) | u == v = Just s
    unify' Nil Nil = Just s
    unify' _ _ = Nothing

show_st (s,c) =
  let reified = sortBy (\(x,_) (y,_) -> if x < y then LT else if x == y then EQ else GT) $
                       map (\(x,v) -> (x, walk' v s)) s
  in show (reified, c)

-- program evaluates to a stream of states which are pairs of substitution and
-- auxilary variable counter.
eval x st@(s,c) =
  case x of
    (Uni t t') ->
      let s' = unify t t' s
          unit (s,c) = Mature (s,c) Empty
      in
      case s' of
        Nothing -> mzero
        Just s' -> unit (s',c)
    (Disj g g') -> eval g st `mplus` eval g' st
    (Conj g g') -> eval g st `bind` \st -> eval g' st
    (Fresh f) -> eval (f (var c)) (s, c + 1)
    (Fun _ a) -> eval a st
    (Call f _) -> eval f st
    (Zzz a) -> Immature (eval a st)

reify' v stream =
  let map' f Empty = []
      map' f (Mature x xs) = f x : map' f xs
  in map' (\(s,c) -> reify v s) stream

walk' v s =
  case walk v s of
    Var u -> Var u
    Pair v u -> Pair (walk' v s) (walk' u s)
    u -> u

reify v s =
  let
      u = walk' v s

      reify_s v s =
        case walk v s of
          Var v -> ext_s v (reify_name $ length s) s
          Pair v u -> reify_s u (reify_s v s)
          _ -> s

      reify_name = R -- temporary solution. need to get rid of it
        --"_." ++ (show n)
  in walk' u (reify_s u empty_subst)
