{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
module AnnotatedDef where

import           Text.Printf (printf)
import           AnnotationType

data AnnotatedDef g a = AnnotatedDef
  { getName :: String
  , getArgs :: [a]
  , getBody :: g a
  , getAnnotations :: [AnnotationType]
  }
  deriving (Eq, Ord)

instance Functor g => Functor (AnnotatedDef g) where
  fmap :: Functor g => (a -> b) -> AnnotatedDef g a -> AnnotatedDef g b
  fmap f (AnnotatedDef name args body annotations) = AnnotatedDef name (fmap f args) (fmap f body) annotations

instance (Show a, Show (g a)) => Show (AnnotatedDef g a) where
  show :: Show a => AnnotatedDef g a -> String
  show (AnnotatedDef name args body annotations) = printf "filter (%s) \n %s %s = %s" (unwords $ map show annotations) name (unwords $ map show args) (show body)
