{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module BTA.SizeConversion where

import           Data.Group
import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Semigroup     (sconcat)
import           BTA.AnnotatedProgram
import           BTA.AnnotatedDef
import           BTA.InvokeAnnotation
import qualified Syntax
import qualified Data.Map            as Map
import Syntax    (Dot, dot)


data AbstractTerm a = Sum 
    { getN :: Int
    , getMap :: Map.Map a Int
    } 
    deriving (Eq, Ord)

fromVar :: Ord a => a -> AbstractTerm a 
fromVar var = Sum 0 $ Map.fromList [(var, 1)]

isLess :: Ord a => AbstractTerm a -> AbstractTerm a -> Bool
isLess t1@(Sum n1 mp1) t2@(Sum n2 mp2) = 
  let (Sum n3 mp3) = t2 ~~ t1 in 
  n3 >= 0 && (all (0 <= ) $ Map.elems mp3) && (n3 > 0 || (any (0 < ) $ Map.elems mp3))

isPositive :: Ord a => AbstractTerm a -> Bool
isPositive = isLess mempty

instance Ord a => Semigroup (AbstractTerm a) where
  (<>) :: AbstractTerm a -> AbstractTerm a -> AbstractTerm a
  (Sum a1 mp1) <> (Sum a2 mp2) =
    Sum (a1 + a2) $ Map.filter (/= 0) $ Map.unionWith (+) mp1 mp2

instance Ord a => Monoid (AbstractTerm a) where
  mempty :: AbstractTerm a 
  mempty = Sum 0 Map.empty

instance Ord a => Group (AbstractTerm a) where 
  invert :: AbstractTerm a -> AbstractTerm a 
  invert (Sum n mp) = Sum (-n) $ negate <$> mp

instance Show a => Show (AbstractTerm a) where
  show (Sum a mp) = show a ++ intercalate " + " (map show $ Map.toList mp)

instance Dot a => Dot (AbstractTerm a) where
  dot (Sum a mp) = dot a ++ intercalate " + " (map dot $ Map.toList mp)

termfmap :: Ord b => (a -> b) -> AbstractTerm a -> AbstractTerm b
termfmap f (Sum a mp) = Sum a $ Map.mapKeys f mp

convert :: AnnotatedProgram (AnnG Syntax.Term) String -> AnnotatedProgram (AnnG AbstractTerm) String
convert (AnnotatedProgram defs goal) = AnnotatedProgram (map convertDef defs) (convertGoal goal)

convertDef :: Ord a => AnnotatedDef (AnnG Syntax.Term) a -> AnnotatedDef (AnnG AbstractTerm) a
convertDef (AnnotatedDef name args body annotations) =
    AnnotatedDef name args (convertGoal body) annotations

convertGoal :: Ord a => AnnG Syntax.Term a -> AnnG AbstractTerm a
convertGoal (Conjunction g1 g2 gl) = Conjunction (convertGoal g1) (convertGoal g2) (map convertGoal gl)
convertGoal (Disjunction g1 g2 gl) = Disjunction (convertGoal g1) (convertGoal g2) (map convertGoal gl)
convertGoal (Fresh name g) = Fresh name (convertGoal g)
convertGoal (Invoke name lst ann) = Invoke name (map convertTerm lst) ann
convertGoal (Delay g) = Delay (convertGoal g)
convertGoal (g1 :=: g2) = convertTerm g1 :=: convertTerm g2

convertTerm :: Ord a => Syntax.Term a -> AbstractTerm a
convertTerm (Syntax.V v) = Sum 0 (Map.insert v 1 Map.empty)
convertTerm (Syntax.C name lst) = 
  sconcat $ (Sum 1 Map.empty) :| (map convertTerm lst)