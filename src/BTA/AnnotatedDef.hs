{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BTA.AnnotatedDef where

import           Text.Printf (printf)
import           BTA.AnnotationType
import           BTA.InvokeAnnotation (AnnG, annotateInvokes)
import           Syntax (Dot, dot, showVar, G, Term)
import Debug.Trace (trace)

data AnnotatedDef g a = AnnotatedDef
  { getName :: String
  , getArgs :: [a]
  , getBody :: g a
  , getAnnotations :: [AnnotationType]
  }
  deriving (Eq, Ord)
  
instance Functor g => Functor (AnnotatedDef g) where
  fmap :: (a -> b) -> AnnotatedDef g a -> AnnotatedDef g b
  fmap f (AnnotatedDef name args body annotations) = AnnotatedDef name (fmap f args) (fmap f body) annotations

instance (Show a, Show (g a)) => Show (AnnotatedDef g a) where
  show :: AnnotatedDef g a -> String
  show (AnnotatedDef name args body annotations) = printf "filter (%s) \n %s %s = %s;" (unwords $ map show annotations) name (unwords $ map show args) (show body)

instance {-# OVERLAPPING #-} (Show (g String)) => Show (AnnotatedDef g String) where
  show :: AnnotatedDef g String -> String
  show (AnnotatedDef name args body annotations) = printf "filter (%s) \n %s %s = %s;" (unwords $ map show annotations) name (unwords args) (show body)

instance (Dot a, Dot (g a)) => Dot (AnnotatedDef g a) where
  dot :: AnnotatedDef g a -> String
  dot (AnnotatedDef name args body annotations) = printf "filter (%s) \n %s %s = %s;" (unwords $ map dot annotations) name (unwords $ map dot args) (dot body)

annotateInvokesDef :: AnnotatedDef Syntax.G String -> AnnotatedDef (AnnG Term) String
annotateInvokesDef (AnnotatedDef name args body annotations) =
  AnnotatedDef name args (annotateInvokes body) annotations
