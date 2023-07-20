{-# LANGUAGE InstanceSigs #-}
module AnnotatedProgram where

import           AnnotatedDef

data AnnotatedProgram g a = AnnotatedProgram
  { getDefs :: [AnnotatedDef g a]
  , getGoal :: g a
  }
  deriving (Show, Eq)

instance Functor g => Functor (AnnotatedProgram g) where
  fmap :: Functor g => (a -> b) -> AnnotatedProgram g a -> AnnotatedProgram g b
  fmap f (AnnotatedProgram defs goal) = AnnotatedProgram (map (fmap f) defs) (fmap f goal)

instance Semigroup (AnnotatedProgram g a) where
  (<>) :: AnnotatedProgram g a -> AnnotatedProgram g a -> AnnotatedProgram g a
  AnnotatedProgram defs1 goal1 <> AnnotatedProgram defs2 _ =
    AnnotatedProgram (defs1 <> defs2) goal1
