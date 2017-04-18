{- 
   Implementation of MicroKanren as in the paper 
   "µKanren: A Minimal Functional Core for Relational Programming"
   by Jason Hemann and Daniel P. Friedman
-}

module MuKanren where

type Var = Integer
-- for now, only integer atoms are allowed
data Term = Var Var | Pair Term Term | Atom Integer deriving Show

data Stream a = Empty 
              | Mature a (Stream a) 
           -- | Immature (Stream a) -- do we need this?
              deriving Show 

type Name = String 
data AST = Uni Term Term | Conj AST AST | Disj AST AST | Fresh (Term -> AST) | Fun Name AST

-- Combinators to write programs with
var = Var
at = Atom

(===) = Uni
(|||) = Disj
(&&&) = Conj
call_fresh = Fresh
fun = Fun 

empty_state = ([], 0)

mzero = Empty

-- used for disjunctions, interleaves streams
mplus Empty _2 = _2 
mplus (Mature h tl) _2 = Mature h (_2 `mplus` tl)
--  mplus (Immature _1) _2 = Immature (_1 `mplus` _2)

-- used for conjuctions
bind Empty g = mzero
bind (Mature x xs) g = g x `mplus` bind xs g
--  bind (Immature _1) _2 = Immature (bind _1 _2)

unify u v s = 
  -- do we need to add occurs check?
  unify' (walk u s) (walk v s) 
  where 
    ext_s u v s = (u, v) : s

    walk (Var v) s = 
      case lookup v s of 
        Nothing -> Var v
        Just t  -> walk t s
    walk u _ = u

    unify' (Var u) (Var v) | u == v = Just s 
    unify' (Var u) _ = Just (ext_s u v s)
    unify' _ (Var v) = Just (ext_s v u s)
    unify' (Pair u u') (Pair v v') = 
      case unify u v s of 
        Nothing -> Nothing
        Just s' -> unify u' v' s'
    unify' (Atom u) (Atom v) | u == v = Just s
    unify' _ _ = Nothing

-- program evaluates to a stream of states which are pairs of substitution and 
-- auxilary variable counter.
eval a s = 
  eval' a s 
  where 
    eval' (Uni t t') = \(s,c) -> 
      let s' = unify t t' s 
          unit = \(s,c) -> Mature (s,c) Empty
      in
      case s' of 
        Nothing -> mzero
        Just s' -> unit (s',c)
    eval' (Disj g g') = \st -> (mplus (eval g st) (eval g' st))
    eval' (Conj g g') = \st -> bind (eval g st) (\st -> eval g' st) 
    eval' (Fresh f) = \(s, c) -> eval (f (var c)) (s, c + 1)
    eval' (Fun _ a) = eval a
