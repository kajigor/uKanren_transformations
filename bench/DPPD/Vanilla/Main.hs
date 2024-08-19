{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)
import Term

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified Vanilla_simple
import qualified Vanilla_online
import qualified Vanilla_offline

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)


appGen :: (MonadPlus m) => m Term
appGen = do
  l1 <- listGen
  l2 <- listGen
  l3 <- listGen
  return (App l1 l2 l3)


listAppGen :: (MonadPlus m) => m Term
listAppGen = do 
  app <- appGen
  return Nil <|> (Cons app <$> listAppGen)

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval3 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> m r) -> (x1, x2, x3) -> [r]
eval3 listify f = eval listify  $ \(y1, y2, y3) -> f y1 y2 y3 

eval8 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8) -> [r]
eval8 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7, y8) -> f y1 y2 y3 y4 y5 y6 y7 y8

main = defaultMain
  [
    bgroup "vanilla"
     [
        bench "offlineOOO"  $ nf (eval0 (takeS 10) Vanilla_offline.solvesOOO) ()
      , bench "onlineOOO"   $ nf (eval3 (takeS 10) Vanilla_online.solveOOO) (listGen, listGen, listGen)
      -- , bench "simpleOOO"   $ nf (eval8 (takeS 10) Vanilla_simple.helpOOO) (listGen, listGen, listGen, listGen, appGen, listGen, appGen, listAppGen)
     ]
  ]

--main =
--  print $ (takeS 1) $ RelSort_cpd_ans.sortoO sortGen sortGen