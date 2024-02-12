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
    = Cabbage
    | Cons Term Term
    | Empty
    | False
    | Goat
    | Man
    | Nil
    | Pair Term Term
    | Quad Term Term Term Term
    | True
    | Wolf
    deriving (Show, Eq, Generic, DS.NFData)

genBool :: MonadPlus m => m Term
genBool = return Term.True <|> return Term.False

gen :: MonadPlus m => m Term
gen = (Pair <$> (Quad <$> genBool <*> genBool <*> genBool <*> genBool) <*> (Quad <$> genBool <*> genBool <*> genBool <*> genBool))
