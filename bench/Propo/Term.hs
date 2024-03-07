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
    = Conj Term Term
    | Disj Term Term
    | Falso
    | Impl Term Term
    | Lit Term
    | Neg Term
    | Succ Term
    | Trueo
    | Var Term
    | Zero
    deriving (Show, Eq, Generic, DS.NFData)