{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)
import Term

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified Vanilla_online
import qualified Vanilla_unfold

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

rightSort :: Term
rightSort = Cons n2 (Cons n1 (Cons n4 (Cons n0 (Cons n1 Nil))))

wrongSort :: Term
wrongSort = Cons n2 (Cons n1 (Cons n2 (Cons n0 (Cons n1 Nil))))

resSort0 :: Term
resSort0 = Cons n0 (Cons n1 (Cons n2 (Cons n3 (Cons n4 (Cons n5 (Cons n6 Nil))))))

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, xx9, x10) -> [r]
eval2 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) -> f y1 y2 y3 y4 y5 y6 y7 y8 y9 u1

resSort = eval (takeS 1) RelSort_Unfold.sortoI rightSort
resSort1 = eval (takeS 1) RelSort_Unfold.sortoI wrongSort

main = defaultMain
  [
    bgroup "vanilla"
     [
        bench "offlineOOO"  $ nf (eval10 (takeS 1) Vanilla_unfold.solveOOO) (natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)
      , bench "onlineOOO"   $ nf (eval0 (takeS 1) Vanilla_online.solveOOO) ()
     ]
  ]

--main =
--  print $ (takeS 1) $ RelSort_cpd_ans.sortoO sortGen sortGen