module Data where

data Def = Def { name :: String, args :: [String], body :: Goal }

data Spec = Spec { defs :: [Def], goal :: Goal }

data Term = Var String
          | Ctor String [Term]
          | Free Integer
          deriving Eq

data Goal = Unify Term Term
          | Disj Goal Goal
          | Conj Goal Goal
          | Fresh String Goal
          | Zzz Goal
          | Invoke String [Term]
          deriving Eq

data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion
              | Immature (Stream a)

type Subst = [(Integer, Term)]

data State = State { getSubst :: Subst, getState :: String -> Term,
                     index :: Integer, vars :: [String] }


type Ctx = [Goal]

data Tree = Success  { state :: State}
          | Fail
          | Step     { getInd :: Integer, state :: State, getGoal :: Goal, freshVars :: [Integer], getChild :: Tree }
          | Or       { getInd :: Integer, state :: State, getGoal :: Goal, getChildren :: [Tree] }
          | Split    { getInd :: Integer, state :: State, getLGoal :: Goal, getRGoal :: Goal, getLChild :: Tree, getRChild :: Tree }
          | Renaming { getInd :: Integer, state :: State, getGoal :: Goal }
          | Gen      { getInd :: Integer, state :: State {- substitution :: [(Term, Term)] -} , getGoal :: Goal, getChild :: Tree }

type Renaming = [(Term, Term)]
