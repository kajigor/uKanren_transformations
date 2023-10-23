{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)
import Term 

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import RelSort_unfold (sortoIOffline, sortoOOffline)
--import RelSort_cpd_ans (sortoIOnline, sortoOOnline)


natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

rightSort :: Term
rightSort = Cons (Succ (Succ Zero)) (Cons Zero (Cons Zero (Cons (Succ Zero) Nil)))

wrongSort :: Term
wrongSort = Cons (Succ (Succ (Succ Zero))) (Cons Zero (Cons Zero (Cons (Succ Zero) Nil)))

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval8 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8) -> [r]
eval8 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7, y8) -> f y1 y2 y3 y4 y5 y6 y7 y8

eval6 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> m r) -> (x1, x2, x3, x4, x5, x6) -> [r]
eval6 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6) -> f y1 y2 y3 y4 y5 y6

eval14 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) -> [r]
eval14 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) -> f y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14

resSort = eval (takeS 1) sortoIOffline rightSort
resSort1 = eval (takeS 1) sortoIOffline wrongSort
nat8 = (natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)


main = defaultMain
  [
    bgroup "RelSort"
     [
        bench "offline1"    $ nf (eval (takeS 1) sortoIOffline) $ traceShow resSort1 rightSort
      , bench "offline2"    $ nf (eval8 (takeS 1) sortoOOffline) $ traceShow (eval8 (takeS 4) sortoOOffline nat8) nat8
--      , bench "online1"     $ nf (eval6 (takeS 1) sortoIOnline) rightSort
     ]
  ]