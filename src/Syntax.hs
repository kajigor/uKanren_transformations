{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax where

import Data.List
import Text.Printf
import Data.Char
-- import qualified Data.Map as Map

type X    = String -- Syntactic variables
type S    = Int    -- Semantic variables
type Name = String -- Names of variables/definitions

-- Terms
data Term v = V v | C String [Term v] deriving (Eq, Ord)
type Tx     = Term X
type Ts     = Term S

instance Functor Term where
  fmap f (V v)    = V $ f v
  fmap f (C s ts) = C s $ map (fmap f) ts

-- Definitions
data Def = Def Name [Name] (G X)
         deriving (Eq, Ord)

instance Show Def where
  show (Def name args body) = printf "%s %s = %s" name (unwords args) (show body)

-- type DefMap = Map.Map (Name, Int) Def

-- insertDef :: Def -> DefMap -> DefMap
-- insertDef def@(Def n a _) = Map.insert (n, length a) def

-- defMapFromList :: [Def] -> DefMap
-- defMapFromList = foldr insertDef Map.empty

data Program = Program [Def] (G X)
             deriving (Show, Eq)

-- Goals
data G a =
    Term a :=: Term a
  | G a :/\: G a
  | G a :\/: G a
  | Fresh  Name (G a)
  | Invoke Name [Term a]
  | Let Def (G a) deriving (Eq, Ord)

instance Functor G where
  fmap f (t :=: u)          = (f <$> t) :=:  (f <$> u)
  fmap f (g :/\: h)         = (f <$> g) :/\: (f <$> h)
  fmap f (g :\/: h)         = (f <$> g) :\/: (f <$> h)
  fmap f (Fresh name g)     = Fresh name (f <$> g)
  fmap f (Invoke name args) = Invoke name $ map (f <$>) args
  fmap f (Let def g)        = Let def (f <$> g)

freshVars :: [Name] -> G t -> ([Name], G t)
freshVars names (Fresh name goal) = freshVars (name : names) goal
freshVars names goal = (names, goal)

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
  go (Let _ g) = go g

fvg :: G X -> [X]
fvg = nub . go
 where
  go (t1 :=:  t2) = fv t1 ++ fv t2
  go (g1 :/\: g2) = go g1 ++ go g2
  go (g1 :\/: g2) = go g1 ++ go g2
  go (Invoke _ ts) = concatMap fv ts
  go (Fresh x g)   = filter (x /=) $ go g
  go (Let _ g) = go g

subst_in_term :: Eq v => v -> Term v -> Term v -> Term v
subst_in_term v t t0@(V v0)     = if v == v0 then t else t0
subst_in_term v t    (C n args) = C n $ map (subst_in_term v t) args

subst_in_goal :: X -> Term X -> G X -> G X
subst_in_goal v t   (t1 :=:  t2)        = subst_in_term v t t1 === subst_in_term v t t2
subst_in_goal v t   (g1 :/\: g2)        = subst_in_goal v t g1 &&& subst_in_goal v t g2
subst_in_goal v t   (g1 :\/: g2)        = subst_in_goal v t g1 ||| subst_in_goal v t g2
subst_in_goal v t g@(Fresh n g')        = if v == n then g else Fresh n $ subst_in_goal v t g'
subst_in_goal v t   (Invoke n ts)       = Invoke n $ map (subst_in_term v t) ts
subst_in_goal v t   (Let (Def n a g1) g2) =
  Let (Def n a (if elem v a then g1 else subst_in_goal v t g1)) $ subst_in_goal v t g2

instance Show a => Show (Term a) where
  show (V v) = showVar v
  show (C name []) | isNil name = "[]"
  show (C name [h, t]) | isCons name = printf "(%s : %s)" (show h) (show t)
  show c | isSucc c || isZero c = pretifyNum 0 c show showVar
  show (C name ts) =
            case ts of
              [] -> name
              _  -> printf "C %s [%s]" name (intercalate ", " $ map show ts)

instance Show a => Show (G a) where
  show (t1 :=:  t2)               = printf "%s = %s" (show t1) (show t2)
  show (g1 :/\: g2)               = printf "(%s /\\ %s)" (show g1) (show g2)
  show (g1 :\/: g2)               = printf "(%s \\/ %s)" (show g1) (show g2)
  show (Fresh name g)             =
    let (names, goal) = freshVars [name] g in
    printf "fresh %s (%s)" (unwords $ map show $ reverse names) (show goal)
  show (Invoke name ts)           = printf "%s %s" name (unwords $ map (\x -> if ' ' `elem` x then printf "(%s)" x else x) $ map show ts)
  show (Let (Def name args body) g) = printf "let %s %s = %s in %s" name (unwords args) (show body) (show g)

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
  dot c | isSucc c || isZero c = pretifyNum' 0 c dot dotVar
  dot (C name [x, y]) | isPair name = printf "(%s, %s)" (dot x) (dot y)
  dot (C name ts) =
          case ts of
            [] -> name
            _  -> printf "C %s [%s]" name (unwords $ map dot ts)

isNil s = map toLower s == "nil" || s == "[]"
isCons s = map toLower s == "cons" || s == "%"
isZero (C o []) = let l = map toLower o in l == "o" || l == "z"
isZero _ = False
isSucc (C s [n]) = map toLower s == "s"
isSucc _ = False
isPair s = map toLower s == "pair"

dotVar :: Dot a => a -> String
dotVar = printf "v<SUB>%s</SUB>" . dot

showVar :: Show a => a -> String
showVar = printf "v.%s" . show

predec :: Term a -> Term a
predec c@(C _ [a]) | isSucc c = a
predec c = error $ printf "Failed to get predecessor"

pretifyNum :: Show a => Int -> Term a -> (Int->String) -> (a -> String) -> String
pretifyNum acc (V v) intPrint varPrint = printf "(%s + %s)" (intPrint acc) (varPrint v)
pretifyNum acc c intPrint varPrint | isSucc c = pretifyNum (1 + acc) (predec c ) intPrint varPrint
pretifyNum acc c intPrint _ | isZero c = intPrint acc
pretifyNum acc c intPrint varPrint = error (printf "Failed to pretifyNum: %s" (show c))

pretifyNum' :: Dot a => Int -> Term a -> (Int->String) -> (a -> String) -> String
pretifyNum' acc (V v) intPrint varPrint = printf "(%s + %s)" (intPrint acc) (varPrint v)
pretifyNum' acc c intPrint varPrint | isSucc c = pretifyNum' (1 + acc) (predec c ) intPrint varPrint
pretifyNum' acc c intPrint _ | isZero c = intPrint acc
pretifyNum' acc c intPrint varPrint = error (printf "Failed to pretifyNum': %s" (dot c))

instance Dot a => Dot (G a) where
  dot (t1 :=:  t2)               = printf "%s = %s" (dot t1) (dot t2)
  dot (g1 :/\: g2)               = printf "(%s /\\ %s)" (dot g1) (dot g2)
  dot (g1 :\/: g2)               = printf "(%s \\/ %s)" (dot g1) (dot g2)
  dot (Fresh name g)             =
    let (names, goal) = freshVars [name] g in
    printf "fresh %s (%s)" (dot $ reverse names) (dot goal)
  dot (Invoke name ts)           = printf "%s(%s)" name (dot ts)
  dot (Let (Def name args body) g) = printf "let %s = %s in %s" name (unwords args) (dot body) (dot g)

instance {-# OVERLAPPING #-} Dot a => Dot [G a] where
  dot gs = intercalate " /\\ " (map dot gs)
