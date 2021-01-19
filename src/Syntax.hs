{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax where

import Data.List ( intercalate, nub )
import Text.Printf ( printf )
import Data.Char ( toLower )

type X    = String -- Syntactic variables
type S    = Int    -- Semantic variables
type Name = String -- Names of variables/definitions

-- Terms
data Term v = V v | C String [Term v] deriving (Eq, Ord, Functor)
type Tx     = Term X
type Ts     = Term S

-- Definitions
data Def = Def Name [Name] (G X)
         deriving (Eq, Ord)

instance Show Def where
  show (Def name args body) = printf "%s %s = %s" name (unwords args) (show body)

data Program = Program [Def] (G X)
             deriving (Show, Eq)

-- Goals
data G a
  = Term a :=: Term a
  | G a :/\: G a
  | G a :\/: G a
  | Fresh  Name (G a)
  | Invoke Name [Term a]
  deriving (Eq, Ord, Functor)

freshVars :: [Name] -> G t -> ([Name], G t)
freshVars names (Fresh name goal) = freshVars (name : names) goal
freshVars names goal = (reverse names, goal)

infix  8 :=:
infixr 7 :/\:
infixr 6 :\/:

infixr 7 &&&
infixr 6 |||
infix  8 ===

(===) :: Term a -> Term a -> G a
(===) = (:=:)

(|||) :: G a -> G a -> G a
(|||) = (:\/:)

(&&&) :: G a -> G a -> G a
(&&&) = (:/\:)


fresh :: [Name] -> G a -> G a
fresh xs g = foldr Fresh g xs

call :: Name -> [Term a] -> G a
call = Invoke

disj :: [G a] -> Maybe (G a)
disj [] = Nothing
disj xs = Just $ foldr1 (:\/:) xs

conj :: [G a] -> Maybe (G a)
conj [] = Nothing
conj xs = Just $ foldr1 (:/\:) xs

unsafeConj :: [G a] -> G a
unsafeConj = foldr1 (:/\:)

unsafeDisj :: [G a] -> G a
unsafeDisj = foldr1 (:\/:)

successName :: String
successName = "success"

success :: G a
success = Invoke successName []

isSuccess :: G a -> Bool
isSuccess (Invoke successName []) = True
isSuccess _ = False

failureName :: String
failureName = "fail"

failure :: G a
failure = Invoke failureName []

isFailure :: G a -> Bool
isFailure (Invoke failureName []) = True
isFailure _ = False

-- Free variables
fv :: Eq v => Term v -> [v]
fv t = nub $ go t where
  go (V v)    = [v]
  go (C _ ts) = concatMap go ts

fvgs :: G S -> [S]
fvgs = nub . go
 where
  go (t1 :=:  t2) = fv t1 ++ fv t2
  go (g1 :/\: g2) = go g1 ++ go g2
  go (g1 :\/: g2) = go g1 ++ go g2
  go (Invoke _ ts) = concatMap fv ts
  -- go (Fresh x g)   = filter (x /=) $ go g

fvg :: G X -> [X]
fvg = nub . go
 where
  go (t1 :=:  t2) = fv t1 ++ fv t2
  go (g1 :/\: g2) = go g1 ++ go g2
  go (g1 :\/: g2) = go g1 ++ go g2
  go (Invoke _ ts) = concatMap fv ts
  go (Fresh x g)   = filter (x /=) $ go g

substInTerm :: Eq v => v -> Term v -> Term v -> Term v
substInTerm v t t0@(V v0)     = if v == v0 then t else t0
substInTerm v t    (C n args) = C n $ map (substInTerm v t) args

substInGoal :: X -> Term X -> G X -> G X
substInGoal v t   (t1 :=:  t2)  = substInTerm v t t1 === substInTerm v t t2
substInGoal v t   (g1 :/\: g2)  = substInGoal v t g1 &&& substInGoal v t g2
substInGoal v t   (g1 :\/: g2)  = substInGoal v t g1 ||| substInGoal v t g2
substInGoal v t g@(Fresh n g')  = if v == n then g else Fresh n $ substInGoal v t g'
substInGoal v t   (Invoke n ts) = Invoke n $ map (substInTerm v t) ts

instance Show a => Show (Term a) where
  show (V v) = showVar v
  show (C name []) | isNil name = "[]"
  show (C name [h, t]) | isCons name = printf "(%s : %s)" (show h) (show t)
  show c | isSucc c || isZero c = prettifyNum 0 c show showVar
  show (C name ts) =
    case ts of
      [] -> name
      _  -> printf "C %s [%s]" name (intercalate ", " $ map show ts)

instance Show a => Show (G a) where
  show (t1 :=:  t2) = printf "%s = %s" (show t1) (show t2)
  show (g1 :/\: g2) = printf "(%s /\\ %s)" (show g1) (show g2)
  show (g1 :\/: g2) = printf "(%s \\/ %s)" (show g1) (show g2)
  show (Fresh name g) =
    let (names, goal) = freshVars [name] g in
    printf "fresh %s (%s)" (unwords $ map show names) (show goal)
  show (Invoke name ts) =
    printf "%s %s" name (unwords $ map (\x -> if ' ' `elem` x then printf "(%s)" x else x) $ map show ts)

class Dot a where
  dot :: a -> String

instance {-# OVERLAPPING #-} Dot String where
  dot = id

instance Dot Int where
  dot = show

instance (Dot a, Dot b) => Dot (a, b) where
  dot (x,y) = printf "(%s, %s)" (dot x) (dot y)

instance {-# OVERLAPS #-} Dot a => Dot [a] where
  dot x = intercalate ", " (map dot x)

instance Dot a => Dot (Term a) where
  dot (V v) = dotVar v
  dot (C name []) | isNil name = "[]"
  dot (C name [h, C t []]) | isCons name && isNil t = printf "[%s]" (dot h)
  dot (C name [h, t]) | isCons name = printf "%s : %s" (dot h) (dot t)
  dot c | isSucc c || isZero c = prettifyNum' 0 c dot dotVar
  dot (C name [x, y]) | isPair name = printf "(%s, %s)" (dot x) (dot y)
  dot (C name ts) =
          case ts of
            [] -> name
            _  -> printf "C %s [%s]" name (unwords $ map dot ts)

isNil :: [Char] -> Bool
isNil s = map toLower s == "nil" || s == "[]"

isCons :: [Char] -> Bool
isCons s = map toLower s == "cons" || s == "%"

isZero :: Term v -> Bool
isZero (C o []) = let l = map toLower o in l == "o" || l == "z"
isZero _ = False

isSucc :: Term v -> Bool
isSucc (C s [n]) = map toLower s == "s"
isSucc _ = False

isPair :: [Char] -> Bool
isPair s = map toLower s == "pair"

dotVar :: Dot a => a -> String
dotVar = printf "v<SUB>%s</SUB>" . dot

showVar :: Show a => a -> String
showVar = printf "v.%s" . show

predec :: Term a -> Term a
predec c@(C _ [a]) | isSucc c = a
predec c = error $ printf "Failed to get predecessor"

prettifyNum :: Show a => Int -> Term a -> (Int->String) -> (a -> String) -> String
prettifyNum acc (V v) intPrint varPrint = printf "(%s + %s)" (intPrint acc) (varPrint v)
prettifyNum acc c intPrint varPrint | isSucc c = prettifyNum (1 + acc) (predec c ) intPrint varPrint
prettifyNum acc c intPrint _ | isZero c = intPrint acc
prettifyNum acc c intPrint varPrint = error (printf "Failed to prettifyNum: %s" (show c))

prettifyNum' :: Dot a => Int -> Term a -> (Int->String) -> (a -> String) -> String
prettifyNum' acc (V v) intPrint varPrint = printf "(%s + %s)" (intPrint acc) (varPrint v)
prettifyNum' acc c intPrint varPrint | isSucc c = prettifyNum' (1 + acc) (predec c ) intPrint varPrint
prettifyNum' acc c intPrint _ | isZero c = intPrint acc
prettifyNum' acc c intPrint varPrint = error (printf "Failed to prettifyNum': %s" (dot c))

instance Dot a => Dot (G a) where
  dot (t1 :=:  t2) = printf "%s = %s" (dot t1) (dot t2)
  dot (g1 :/\: g2) = printf "(%s /\\ %s)" (dot g1) (dot g2)
  dot (g1 :\/: g2) = printf "(%s \\/ %s)" (dot g1) (dot g2)
  dot (Fresh name g) =
    let (names, goal) = freshVars [name] g in
    printf "fresh %s (%s)" (dot names) (dot goal)
  dot (Invoke name ts) = printf "%s(%s)" name (dot ts)

instance {-# OVERLAPPING #-} Dot a => Dot [G a] where
  dot gs = intercalate " /\\ " (map dot gs)
