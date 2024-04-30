{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow, trace)
import Term
import qualified Contains1_offline
import qualified Contains1_online
import qualified Contains1_simple

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

eval8 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8) -> [r]
eval8 listify f = eval listify $ \(x1, x2, x3, x4, x5, x6, x7, x8) -> f x1 x2 x3 x4 x5 x6 x7 x8

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

term1 = Cons (S O) (Cons O (Cons (S (S O)) Nil))

main :: IO ()
main = defaultMain
  [
    bgroup "Contains1"
     [  bench "onlineO"     $ nf (eval4 (takeS 100) Contains1_online.containsoO) (listGen, natGen, natGen, listGen)
      , bench "offlineO"    $ nf (eval8 (takeS 100) Contains1_offline.containsosdO) (natGen, listGen, listGen, natGen, listGen, listGen, natGen, listGen)
      , bench "simpleO"     $ nf (eval3 (takeS 100) Contains1_simple.containsoIO) (term1, listGen, listGen)
      , bench "onlineO"     $ nf (eval4 (takeS 1000) Contains1_online.containsoO) (listGen, natGen, natGen, listGen) -- failing
      , bench "offlineO"    $ nf (eval8 (takeS 1000) Contains1_offline.containsosdO) (natGen, listGen, listGen, natGen, listGen, listGen, natGen, listGen)
      , bench "simpleO"     $ nf (eval3 (takeS 1000) Contains1_simple.containsoIO) (term1, listGen, listGen)

     ]
  ]
