{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow, trace)
import Term
import qualified Upto_sum2_unfold
import qualified Upto_sum2_cpd_ans

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

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

nat0 = O
nat1 = S nat0
nat2 = S nat1
nat3 = S nat2
nat4 = S nat3

main :: IO ()
main = defaultMain
  [
    bgroup "Upto.sum1"
     [
        bench "offlineIIIO" $ nf (eval3 (takeS 1) Upto_sum2_unfold.sumtrsquaretrIIIO) (nat2, nat1, nat4)
      , bench "onlineIIIO" $ nf (eval4 (takeS 1) Upto_sum2_cpd_ans.sumtrsquaretrIIIO) (nat2, nat1, nat4, natGen)
      , bench "offlineIIOO" $ nf (eval3 (takeS 1) Upto_sum2_unfold.sumtrsquaretrIIOO) (nat2, nat1, natGen)
      , bench "onlineIIOO" $ nf (eval6 (takeS 1) Upto_sum2_cpd_ans.sumtrsquaretrIIOO) (nat2, nat1, natGen, natGen, natGen, natGen)
      , bench "offlineIIOI" $ nf (eval4 (takeS 1) Upto_sum2_unfold.sumtrsquaretrIIOI) (nat2, nat1, nat4, natGen)
      , bench "onlineIIOI" $ nf (eval5 (takeS 1) Upto_sum2_cpd_ans.sumtrsquaretrIIOI) (nat2, nat1, nat4, natGen, natGen)
     ]
  ]
