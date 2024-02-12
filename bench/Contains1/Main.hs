{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow, trace)
import Term
import qualified Contains1_unfold1
import qualified Contains1_cpd_ans

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

eval7 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> m r) -> (x1, x2, x3, x4, x5, x6, x7) -> [r]
eval7 listify f = eval listify $ \(x1, x2, x3, x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

-- ? containso [S O, O, S (S O)] str
term1 = Cons (S O) (Cons O (Cons (S (S O)) Nil))

main :: IO ()
main = defaultMain
  [
    bgroup "Contains1"
     [ bench "offlineI"    $ nf (eval (takeS 1) Contains1_unfold1.containsoI) term1,
       bench "onlineI"     $ nf (eval6 (takeS 1) Contains1_cpd_ans.containsoI) (term1, natGen, natGen, natGen, natGen, natGen),
       bench "onlineO"     $ nf (eval7 (takeS 1) Contains1_cpd_ans.containsoO) (natGen, natGen, natGen, natGen, natGen, natGen, natGen),
       bench "offlineO"    $ nf (eval6 (takeS 1) Contains1_unfold1.containsoO) (natGen, natGen, natGen, natGen, natGen, natGen)

     ]
  ]
