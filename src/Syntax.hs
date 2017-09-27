module Syntax where

type X    = String -- Syntactic variables
type S    = Int    -- Semantic variables
type Name = String -- Names of variables/definitions

-- Terms
data Term v = V v | C String [Term v] deriving (Eq, Show)
type Tx     = Term X
type Ts     = Term S

-- Goals
data G = 
    Tx :=: Tx
  | G :/\: G
  | G :\/: G
  | Fresh  Name G 
  | Invoke Name [Tx] deriving Show

infixr 7 &&&
infixr 6 |||
infix  8 ===

(===) = (:=:)
(|||) = (:\/:)
(&&&) = (:/\:)

fresh xs g = foldr Fresh g xs
call       = Invoke

-- Definitions
type Def = (Name, [Name], G)

def = (,,)

-- Specification
type Spec = ([Def], G)

spec = (,)
