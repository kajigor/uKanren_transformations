{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Program where

import           Def

data Program g a = Program
  { getDefs :: [Def g a]
  , getGoal :: g a
  }
  deriving (Eq)

instance Functor g => Functor (Program g) where
  fmap :: Functor g => (a -> b) -> Program g a -> Program g b
  fmap f (Program defs goal) = Program (map (fmap f) defs) (fmap f goal)

instance Semigroup (Program g a) where
  (<>) :: Program g a -> Program g a -> Program g a
  Program defs1 goal1 <> Program defs2 _ =
    Program (defs1 <> defs2) goal1

instance (Show a, Show (g a)) => Show (Program g a) where 
  show :: Program g a -> String 
  show (Program defs goal) = unlines (map show defs) ++ "\n" ++ show goal
  
instance {-# OVERLAPPING #-} (Show (g String)) => Show (Program g String) where
  show :: Program g String -> String 
  show (Program defs goal) = unlines (map show defs) ++ "\n" ++ show goal