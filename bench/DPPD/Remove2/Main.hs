{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import Term
import qualified Remove2_simple
import qualified Remove2_offline
import qualified Remove2_online

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval4 :: (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

-- [Succ Zero, Zero, Succ (Succ Zero)]
term = Cons (Succ Zero) (Cons (Zero) (Cons (Succ (Succ Zero)) Nil))

main = defaultMain
  [
    bgroup "Remove2"
     [
       bench "offlineO" $ nf (eval0 (takeS 1) Remove2_offline.rrdsO) ()
     , bench "onlineO"  $ nf (eval  (takeS 1) Remove2_online.rrO) (listGen)
     , bench "simpleO"  $ nf (eval4 (takeS 1) Remove2_simple.rr) (term, listGen, listGen, natGen)
     ]
  ]