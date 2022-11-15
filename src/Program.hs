{-# LANGUAGE InstanceSigs #-}
module Program where

import Def

data Program g a = Program
  { getDefs :: [Def g a]
  , getGoal :: g a
  }
  deriving (Show, Eq)

instance Functor g => Functor (Program g) where
  fmap :: Functor g => (a -> b) -> Program g a -> Program g b
  fmap f (Program defs goal) = Program (map (fmap f) defs) (fmap f goal)

instance Semigroup (Program g a) where
  (<>) :: Program g a -> Program g a -> Program g a
  Program defs1 goal1 <> Program defs2 _ =
    Program (defs1 <> defs2) goal1
