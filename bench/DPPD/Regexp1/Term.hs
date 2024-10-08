{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Term where

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

data Term
    = Cat Term Term
    | Char Term
    | Cons Term Term
    | Empty
    | Or Term Term
    | Star Term
    | Nil
    | Zero 
    | Succ Term
    deriving (Show, Eq, Generic, DS.NFData)