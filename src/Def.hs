{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE FlexibleInstances#-}

{-# LANGUAGE IncoherentInstances #-}
module Def where

import           Text.Printf (printf)

data Def g a = Def
  { getName :: String
  , getArgs :: [a]
  , getBody :: g a
  }
  deriving (Eq, Ord)

instance Functor g => Functor (Def g) where
  fmap :: Functor g => (a -> b) -> Def g a -> Def g b
  fmap f (Def name args body) = Def name (fmap f args) (fmap f body)

instance (Show a, Show (g a)) => Show (Def g a) where
  show :: Show a => Def g a -> String
  show (Def name args body) = printf "%s %s = %s" name (unwords $ map show args) (show body)

instance {-# OVERLAPPING #-} (Show (g String)) => Show (Def g String) where
  show :: Def g String -> String
  show (Def name args body) = printf "%s %s = %s" name (unwords args) (show body)