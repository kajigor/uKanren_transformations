{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BTA.AnnotatedProgram where

import           BTA.AnnotatedDef
import           BTA.InvokeAnnotation
import           Syntax           (Dot, dot, G, Term)

data AnnotatedProgram g a = AnnotatedProgram
  { getDefs :: [AnnotatedDef g a]
  , getGoal :: g a
  }
  deriving (Eq)

instance Functor g => Functor (AnnotatedProgram g) where
  fmap :: (a -> b) -> AnnotatedProgram g a -> AnnotatedProgram g b
  fmap f (AnnotatedProgram defs goal) = AnnotatedProgram (map (fmap f) defs) (fmap f goal)

instance Semigroup (AnnotatedProgram g a) where
  (<>) :: AnnotatedProgram g a -> AnnotatedProgram g a -> AnnotatedProgram g a
  AnnotatedProgram defs1 goal1 <> AnnotatedProgram defs2 _ =
    AnnotatedProgram (defs1 <> defs2) goal1

instance (Show a, Show (g a)) => Show (AnnotatedProgram g a) where 
  show :: AnnotatedProgram g a -> String 
  show (AnnotatedProgram defs goal) = unlines (map show defs) ++ "\n" ++ show goal
  
instance {-# OVERLAPPING #-} (Show (g String)) => Show (AnnotatedProgram g String) where
  show :: AnnotatedProgram g String -> String 
  show (AnnotatedProgram defs goal) = unlines (map show defs) ++ "\n" ++ show goal
  
  
instance (Dot a, Dot (g a)) => Dot (AnnotatedProgram g a) where 
  dot :: AnnotatedProgram g a -> String 
  dot (AnnotatedProgram defs goal) = unlines (map dot defs) ++ "\n" ++ dot goal


annotateInvokesPr :: AnnotatedProgram Syntax.G String -> AnnotatedProgram (AnnG Term) String
annotateInvokesPr (AnnotatedProgram defs goal) =
  AnnotatedProgram (map annotateInvokesDef defs) (annotateInvokes goal)