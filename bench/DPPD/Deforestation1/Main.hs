{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)


import qualified Deforestation1_offline
import qualified Deforestation1_online
import qualified Deforestation1_simple
import Term

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

firstTerm = Cons (S O) (Cons (S O) (Cons (S O) (Cons (S O) Nil)))

main :: IO ()
main = defaultMain
  [
    bgroup "Deforestation1"
     [ 
        bench "onlineO"     $ nf (eval0 (takeS 1) Deforestation1_online.rrO) ()
      , bench "offlineO"    $ nf (eval0 (takeS 1) Deforestation1_offline.rrsdO) ()
      , bench "simpleO"     $ nf (eval (takeS 1) Deforestation1_simple.rrIO) (firstTerm)
     ]
  ]