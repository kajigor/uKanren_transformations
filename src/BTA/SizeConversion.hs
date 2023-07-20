{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module BTA.SizeConversion where

import           Data.Char          (toLower)
import           Data.List          (intercalate, nub)
import           Data.List.NonEmpty (NonEmpty (..), toList)
import           Text.Printf        (printf)
import           Util.Miscellaneous (parenthesize)

import qualified Syntax
import AnnotatedProgram
import AnnotatedDef
import AnnotationType
import qualified Data.Map as Map
import qualified InvokeAnnotation as Inv
import Data.Semigroup (sconcat)


data AbstractTerm a = 
    Sum Int (Map.Map a Int)
    deriving (Eq, Ord)

isLess :: Ord a => AbstractTerm a -> AbstractTerm a -> Bool
isLess t1@(Sum n1 mp1) t2@(Sum n2 mp2) = 
  let (Sum n3 mp3) = (getWeight t2 t1) in 
  (n3 >= 0 && (all ((<=) 0) (Map.elems mp3))) && (n3 > 0 || (any (0 < ) (Map.elems mp3)))

isPositive :: Ord a => AbstractTerm a -> Bool
isPositive term = isLess (Sum 0 Map.empty) term

equalMaps :: Ord a => AbstractTerm a -> AbstractTerm a -> Bool
equalMaps (Sum n1 mp1) (Sum n2 mp2) = mp1 == mp2

getWeight :: Ord a => AbstractTerm a -> AbstractTerm a -> AbstractTerm a 
getWeight term1@(Sum n1 mp1) term2@(Sum n2 mp2) = term1 <> (negative term2)

negative :: Ord a => AbstractTerm a -> AbstractTerm a 
negative term@(Sum n mp) = (Sum (-n) (fmap negate mp))

difference :: Ord a => AbstractTerm a -> AbstractTerm a -> AbstractTerm a 
difference term1 term2 = term1 <> (negative term2)

instance Ord a => Semigroup (AbstractTerm a) where
  (<>) :: AbstractTerm a -> AbstractTerm a -> AbstractTerm a
  (Sum a1 mp1) <> (Sum a2 mp2) =
    Sum (a1 + a2) (Map.filter (/= 0) (Map.unionWith (+) mp1 mp2))

instance (Show a) => Show (AbstractTerm a) where
    show (Sum a mp) = (show a) ++ (unwords (map (\x -> " + " ++ (show x)) (Map.toList mp)))


termfmap :: Ord b => (a -> b)-> AbstractTerm a -> AbstractTerm b
termfmap f (Sum a mp) = Sum a (Map.mapKeys f mp)

data AbstractG a = 
    AbstractTerm a :=: AbstractTerm a
  | Conjunction (AbstractG a) (AbstractG a) [AbstractG a] -- a list of conjuncts: at least 2 conjuncts should be present
  | Disjunction (AbstractG a) (AbstractG a) [AbstractG a] -- a list of disjuncts: at least 2 disjuncts should be present
  | Fresh a (AbstractG a)
  | Invoke Syntax.Name [AbstractTerm a] Inv.Ann
  | Delay (AbstractG a)
  deriving (Eq)

freshVars :: [a] -> AbstractG a -> ([a], AbstractG a)
freshVars names (Fresh name goal) = freshVars (name : names) goal
freshVars names goal = (reverse names, goal)


instance (Show a) => Show (AbstractG a) where
  show (t1 :=: t2) = printf "%s = %s" (show t1) (show t2)
  show (Conjunction x y gs) = printf "(%s)" (intercalate " /\\ " $ show <$> (x : y : gs))
  show (Disjunction x y gs) = printf "(%s)" (intercalate " \\/ " $ show <$> (x : y : gs))
  show (Fresh name g) =
    let (names, goal) = freshVars [name] g in
    printf "fresh %s (%s)" (unwords $ map show names) (show goal)
  show (Invoke name ts ann) =
    printf "%s %s %s" (show ann) name (unwords $ map (parenthesize . show) ts)
  show (Delay g) = printf "Delay (%s)" (show g)

convert :: (AnnotatedProgram Inv.AnnG String) -> (AnnotatedProgram AbstractG String)
convert (AnnotatedProgram defs goal) = (AnnotatedProgram (map convertDef defs) (convertGoal goal))

convertDef :: Ord a => (AnnotatedDef Inv.AnnG a) -> (AnnotatedDef AbstractG a) 
convertDef (AnnotatedDef name args body annotations) =
    AnnotatedDef name args (convertGoal body) annotations

convertGoal :: Ord a => (Inv.AnnG a) -> (AbstractG a) 
convertGoal (Inv.Conjunction g1 g2 gl) = Conjunction (convertGoal g1) (convertGoal g2) (map convertGoal gl)
convertGoal (Inv.Disjunction g1 g2 gl) = Disjunction (convertGoal g1) (convertGoal g2) (map convertGoal gl)
convertGoal (Inv.Fresh name g) = Fresh name (convertGoal g)
convertGoal (Inv.Invoke name lst ann) = Invoke name (map convertTerm lst) ann
convertGoal (Inv.Delay g) = Delay (convertGoal g)
convertGoal (g1 Inv.:=: g2) = (convertTerm g1) :=: (convertTerm g2)

convertTerm :: Ord a => (Syntax.Term a) -> (AbstractTerm a) 
convertTerm (Syntax.V v) = (Sum 0 (Map.insert v 1 Map.empty))
convertTerm (Syntax.C name lst) = 
  sconcat ((Sum 1 Map.empty) :| (map convertTerm lst))