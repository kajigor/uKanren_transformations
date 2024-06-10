{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified Multiply_online
import qualified Multiply_offline
import qualified Multiply_simple

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: MonadPlus m => (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: MonadPlus m => (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x1, x2) -> f x1 x2

term = (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))

main = defaultMain
  [
    bgroup "multiply"
     [
        bench "offlineO"    $ nf (eval0 (takeS 5) Multiply_offline.multiplyddsOO) ()
      , bench "onlineO"     $ nf (eval0 (takeS 5) Multiply_online.multiplyOO) ()
      , bench "simple0"     $ nf (eval2 (takeS 5) Multiply_simple.multiply) (term, natGen)
     ]
  ]
