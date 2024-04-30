{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Term where

import Stream
import GHC.Generics (Generic)

import qualified Control.DeepSeq as DS
import Control.Monad (msum, guard, MonadPlus)

data Term
    = Cons Term Term
    | Nil
    | S Term
    | O
    deriving (Show, Eq, Generic, DS.NFData)

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)
