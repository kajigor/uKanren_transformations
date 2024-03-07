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
import qualified RelSort_unfold1
import qualified RelSort_simple

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

rightSort :: Term
rightSort = Cons (Succ (Succ Zero)) (Cons Zero (Cons (Succ Zero) (Cons (Succ (Succ (Succ Zero))) Nil)))

wrongSort :: Term
wrongSort = Cons (Succ (Succ (Succ Zero))) (Cons Zero (Cons Zero (Cons (Succ Zero) Nil)))

resSort0 :: Term
resSort0 = Cons (Zero) (Cons (Succ Zero) (Cons (Succ (Succ Zero)) (Cons (Succ (Succ (Succ Zero))) Nil)))

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: Show r => (m r -> [r]) -> (x1 -> x2 -> m r) -> (x1, x2) -> [r]
eval2 listify f = eval listify  $ \(y1, y2) -> f y1 y2

resSort = eval (takeS 1) RelSort_unfold1.sortoI rightSort
resSort1 = eval (takeS 1) RelSort_unfold1.sortoI wrongSort

sortGen :: (MonadPlus m) => m Term
sortGen = do
  x <- natGen
  return Nil <|> (Cons x <$> sortGen)

main = defaultMain
  [
    bgroup "RelSort"
     [
--        bench "offlineI"    $ nf (eval (takeS 1) RelSort_unfold.sortoI) $ traceShow resSort rightSort
--      , bench "offlineO"    $ nf (eval2 (takeS 1) RelSort_unfold.sortoO) (natGen, natGen) -- failing
--      , bench "onlineI"     $ nf (eval (takeS 1) RelSort_cpd_ans.sortoI) $ traceShow resSort1 rightSort
--        bench "det_unfI"     $ nf (eval (takeS 1) RelSort_Det_unf.sortoI) $ traceShow resSort1 rightSort
--      , bench "det_unfO"     $ nf (eval (takeS 1) RelSort_Det_unf.sortoI) $ traceShow resSort1 rightSort
--      , bench "onlineO"     $ nf (eval2 (takeS 1) RelSort_cpd_ans.sortoO) (natGen, natGen) -- failing
        bench "offlineI1"   $ nf (eval (takeS 1) RelSort_unfold1.sortoI) $ traceShow resSort rightSort
      , bench "offlineO1"   $ nf (eval0 (takeS 5) RelSort_unfold1.sortoO) ()
      , bench "onlineI"     $ nf (eval (takeS 1) RelSort_cpd_ans.sortoI) $ traceShow resSort0 rightSort 
      , bench "onlineO"     $ nf (eval2 (takeS 5) RelSort_cpd_ans.sortoO) (sortGen, sortGen) -- падает
      , bench "simpleI"     $ nf (eval2 (takeS 1) RelSort_simple.sortoII) (rightSort, resSort0)
      , bench "simpleO"     $ nf (eval (takeS 5) RelSort_simple.sortoOI) resSort0
     ]
  ]


--main =
--  print $ (takeS 1) $ RelSort_cpd_ans.sortoO natGen natGen