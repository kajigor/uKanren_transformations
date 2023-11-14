{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow, trace)
import Term
import qualified Model_elim_unfold
import qualified Model_elim_cpd_ans


eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x,y,z) -> f x y z

eval4 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> m r) -> (x1, x2, x3, x4) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

eval5 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (x1, x2, x3, x4, x5) -> [r]
eval5 listify f = eval listify $ \(x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5

eval6 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> m r) -> (x1, x2, x3, x4, x5, x6) -> [r]
eval6 listify f = eval listify $ \(x1, x2, x3, x4, x5, x6) -> f x1 x2 x3 x4 x5 x6

eval8 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8) -> [r]
eval8 listify f = eval listify $ \(x1, x2, x3, x4, x5, x6, x7, x8) -> f x1 x2 x3 x4 x5 x6 x7 x8

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

a = Cons (S O) (Cons (S (S O)) Nil)
b = Cons (S (S (S O))) (Cons O (Cons O Nil))
c = Cons (S O) (Cons (S (S O)) (Cons (S (S (S O))) (Cons O (Cons O Nil))))

main :: IO ()
main = defaultMain
  [
    bgroup "Model_elim"
     [
        bench "offlineOOO" $ nf (eval8 (takeS 1) Model_elim_unfold.solveOOO) (natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)
      , bench "onlineOOO" $ nf (eval3 (takeS 1) Model_elim_cpd_ans.solveOOO) (natGen, natGen, natGen)
      , bench "offlineIII" $ nf (eval3 (takeS 1) Model_elim_unfold.solveIII) (a, b, c)
      , bench "onlineIII" $ nf (eval3 (takeS 1) Model_elim_cpd_ans.solveIII) (a, b, c)
     ]
  ]
