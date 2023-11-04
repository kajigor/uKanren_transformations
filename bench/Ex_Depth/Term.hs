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
    = App Term Term Term
    | Cons Term Term
    | Delete Term Term Term
    | InBoth Term Term Term
    | Member Term Term
    | Nil
    | O
    | S Term
    | Test Term Term Term Term
    deriving (Show, Eq, Generic, DS.NFData)