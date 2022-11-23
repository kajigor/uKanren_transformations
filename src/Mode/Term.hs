{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Mode.Term where

import Text.Printf
import Data.List (intercalate)


newtype Var a = Var { getVar :: a }
              deriving (Eq, Ord, Functor)

instance Show a => Show (Var a) where
  show (Var x) = printf "V.%s" $ show x
  showList xs =
      showChar '(' . (go xs ++) . showChar ')'
    where
      go xs = intercalate ", " (map show xs)

data FlatTerm a = FTCon String [Var a]
                | FTVar (Var a)
                deriving (Eq, Ord, Functor)

instance Show a => Show (FlatTerm a) where
  show (FTCon name args) = printf "%s %s" name $ show args
  show (FTVar v) = show v

varsFromTerm :: FlatTerm a -> [Var a]
varsFromTerm (FTVar v) = [v]
varsFromTerm (FTCon _ args) = args