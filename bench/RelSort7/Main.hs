{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)
import Term

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified RelSort_Unfold
import qualified RelSort_cpd_ans
import qualified RelSort_Simple
import qualified RelSort7_Unfold
import qualified RelSort5_Unfold
import qualified RelSort5_cpd_ans

n0 = Zero
n1 = Succ n0
n2 = Succ n1
n3 = Succ n2
n4 = Succ n3
n5 = Succ n4
n6 = Succ n5

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)


sortGen :: (MonadPlus m) => m Term
sortGen = do
  x <- natGen
  return Nil <|> (Cons x <$> sortGen)

rightSort :: Term
rightSort = Cons n2 (Cons n1 (Cons n4 (Cons n0 (Cons n1 Nil))))

wrongSort :: Term
wrongSort = Cons n2 (Cons n1 (Cons n2 (Cons n0 (Cons n1 Nil))))


resSort4 :: Term
resSort4 = Cons n0 (Cons n1 (Cons n2 (Cons n3 (Cons n4 Nil))))

resSort5 :: Term
resSort5 = Cons n0 (Cons n1 (Cons n2 (Cons n3 (Cons n4 (Cons n5 Nil)))))

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: Show r => (m r -> [r]) -> (x1 -> x2 -> m r) -> (x1, x2) -> [r]
eval2 listify f = eval listify  $ \(y1, y2) -> f y1 y2

resSort = eval (takeS 1) RelSort_Unfold.sortoI rightSort
resSort1 = eval (takeS 1) RelSort_Unfold.sortoI wrongSort

main = defaultMain
  [
    bgroup "RelSort7"
     [
        bench "offlineI1"   $ nf (eval (takeS 1) RelSort_Unfold.sortoI) rightSort
      , bench "offlineO1"   $ nf (eval0 (takeS 5) RelSort_Unfold.sortoO) ()
      , bench "onlineI"     $ nf (eval (takeS 1) RelSort_cpd_ans.sortoI) rightSort
      , bench "offline5"    $ nf (eval0 (takeS 5) RelSort5_Unfold.sortodsO) ()
      , bench "offline6"    $ nf (eval0 (takeS 5) RelSort7_Unfold.sortodsO) ()
      , bench "simple5"    $ nf (eval (takeS 5) RelSort_Simple.sortoOI) resSort4
      , bench "simple6"    $ nf (eval (takeS 5) RelSort_Simple.sortoOI) resSort5
      -- , bench "online5"    $ RelSort5_cpd_ans
      , bench "onlineO"     $ nf (eval2 (takeS 5) RelSort_cpd_ans.sortoO) (sortGen, sortGen) -- Error
      -- , bench "simpleO"     $ nf (eval (takeS 5) RelSort_Simple.sortoOI) resSort0 -- timeout
     ]
  ]

--main =
--  print $ (takeS 1) $ RelSort_cpd_ans.sortoO sortGen sortGen