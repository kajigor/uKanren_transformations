{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Syntax where

type X    = String -- Syntactic variables
type S    = Int    -- Semantic variables
type Name = String -- Names of variables/definitions

-- Terms
data Term v = V v | C String [Term v] deriving (Eq, Ord) --, Show)
type Tx     = Term X
type Ts     = Term S

instance {-# OVERLAPPING #-} Show S where 
  show i = "_." ++ show i

instance Show a => Show (Term a) where
  show (V v) = show v
  show (C name ts) = 
    case name of 
      "Nil" -> "[]"
      "Cons" -> let [h,t] = ts
                in show h ++ " : " ++ show t
      _ -> case ts of 
             [] -> name 
             _  -> "C " ++ name ++ " " ++ concatMap show ts

-- Goals
data G a = 
    Term a :=: Term a
  | G a :/\: G a
  | G a :\/: G a
  | Fresh  Name (G a)
  | Invoke Name [Term a] deriving (Eq, Ord) --, Show)

instance Show a => Show (G a) where
  show (t1 :=:  t2) = show t1 ++ " :=: "  ++ show t2
  show (g1 :/\: g2) = "(" ++ show g1 ++ " :/\\: " ++ show g2 ++ ")"
  show (g1 :\/: g2) = "(" ++ show g1 ++ " :\\/: " ++ show g2 ++ ")"
  show (Fresh name g) = "Fresh " ++ name ++ " (" ++ show g ++ ")"
  show (Invoke name ts) = name ++ "(" ++ show ts ++ ")"

infix  8 :=:
infixr 7 :/\:
infixr 6 :\/:

infixr 7 &&&
infixr 6 |||
infix  8 ===

(===) = (:=:)
(|||) = (:\/:)
(&&&) = (:/\:)

fresh xs g = foldr Fresh g xs
call       = Invoke 

-- Definitions
type Def = (Name, [Name], G X)

def = (,,)

-- Specification
type Spec = ([Def], G X)

spec = (,)
