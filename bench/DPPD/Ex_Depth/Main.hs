{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import Term
import qualified Ex_Depth_offline
import qualified Ex_Depth_online
import qualified Ex_Depth_simple

eval0 :: (m r -> [r]) -> (m r) -> a -> [r]
eval0 listify f x = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x1, x2) -> f x1 x2

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x1, x2, x3) -> f x1 x2 x3

eval29 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24 -> x25 -> x26 -> x27 -> x28 -> m r) -> (a, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28) -> [r]
eval29 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28) -> f b y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25 y26 y27 y28

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

gen = listGen <|> natGen

main = defaultMain
  [
    bgroup "Ex_depth"
     [
        bench "offlineOO" $ nf (eval0 (takeS 5) Ex_Depth_offline.solvesdOO) ()
      , bench "onlineOO"  $ nf (eval29 (takeS 5) Ex_Depth_online.solveOO) (
        gen, gen, gen, gen,
        gen, gen, gen, gen,
        gen, gen, gen, gen,
        gen, gen, gen, gen,
        gen, gen, gen, gen,
        gen, gen, gen, gen,
        gen, gen, gen, gen,
        gen
      ) -- failing
      , bench "simpleOO"  $ nf (eval2 (takeS 5) Ex_Depth_simple.helpOO) (listGen, natGen) -- failing
     ]
  ]


