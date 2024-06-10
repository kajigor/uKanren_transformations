{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Term where

--import Test.Tasty.Bench
--import Stream
import GHC.Generics (Generic)
--import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
--import Control.Applicative (Alternative)
--import Debug.Trace (traceShow)

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    | Trueo
    | Falso
--    deriving (Eq)
    deriving (Show, Eq, Generic, DS.NFData)

--instance Show Term where
--  show (Cons a b) = "Cons " ++ show a ++ " (" ++ show b ++ ") "
--  show Nil = "Nil"
--  show (Succ Zero) = "Succ Zero"
--  show (Succ (Succ Zero)) = "Succ (Succ Zero)"
--  show (Succ (Succ (Succ Zero))) = "Succ (Succ (Succ Zero))"
--  show (Succ (Succ (Succ (Succ Zero)))) = "Succ (Succ (Succ (Succ Zero)))"
--  show (Succ (Succ (Succ (Succ (Succ _))))) = "Succ a lot"
--  show Zero = "Zero"
--  show Trueo = "Trueo"
--  show Falso = "Falso"
--  