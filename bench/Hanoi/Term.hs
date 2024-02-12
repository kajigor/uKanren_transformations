{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Term where

--import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)

import qualified Control.DeepSeq as DS
import Control.Monad (msum, guard, MonadPlus)
--import Control.Applicative (Alternative)
--import Debug.Trace (traceShow)

data Term
    = Cons Term Term
    | Nil
    | One
    | Pair Term Term
    | Thr
    | Two
    | O
    | S Term
    deriving (Show, Eq, Generic, DS.NFData)

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

numGen :: (MonadPlus m) => m Term
numGen = return One <|> return Two <|> return Thr