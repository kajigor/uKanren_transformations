module Data where

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

type Subst = [(Integer, Term)]

data State = State { getSubst :: Subst, getState :: String -> Term, index :: Integer }

