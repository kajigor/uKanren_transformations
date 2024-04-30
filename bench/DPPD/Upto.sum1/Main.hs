{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow, trace)

import Term
import qualified Upto_sum1_online
import qualified Upto_sum1_offline
import qualified Upto_sum1_simple

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

term = (S (S (S (S O)))) 

main :: IO ()
main = defaultMain
  [
    bgroup "Upto.sum1"
     [ 
      bench "offlineO" $ nf (eval0 (takeS 1) Upto_sum1_offline.sumsquaresuptosdO) ()
    , bench "onlineO"  $ nf (eval0 (takeS 1) Upto_sum1_online.sumsquaresuptosdO) ()
    , bench "simpleO"  $ nf (eval  (takeS 1) Upto_sum1_simple.sumsquaresupto) (term)
     ]
  ]
