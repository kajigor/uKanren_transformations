{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)
import Term 

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified RelSort_unfold
import qualified RelSort_cpd_ans
import qualified RelSort_Det_unf


natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

rightSort :: Term
rightSort = Cons (Succ (Succ Zero)) (Cons Zero (Cons (Succ Zero) (Cons (Succ (Succ (Succ Zero))) Nil)))

wrongSort :: Term
wrongSort = Cons (Succ (Succ (Succ Zero))) (Cons Zero (Cons Zero (Cons (Succ Zero) Nil)))

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: Show r => (m r -> [r]) -> (x1 -> x2 -> m r) -> (x1, x2) -> [r]
eval2 listify f = eval listify  $ \(y1, y2) -> f y1 y2

resSort = eval (takeS 1) RelSort_unfold.sortoI rightSort
resSort1 = eval (takeS 1) RelSort_unfold.sortoI wrongSort

main = defaultMain
  [
    bgroup "RelSort"
     [
        bench "offline1"    $ nf (eval (takeS 1) RelSort_unfold.sortoI) $ traceShow resSort rightSort
--      , bench "offline2"    $ nf (eval2 (takeS 1) RelSort_unfold.sortoO) (natGen, natGen) -- failing
      , bench "online1"     $ nf (eval (takeS 1) RelSort_cpd_ans.sortoI) $ traceShow resSort1 rightSort
      , bench "det_unf1"     $ nf (eval (takeS 1) RelSort_Det_unf.sortoI) $ traceShow resSort1 rightSort
--      , bench "online2"     $ nf (eval2 (takeS 1) RelSort_cpd_ans.sortoO) (natGen, natGen) -- failing
     ]
  ]