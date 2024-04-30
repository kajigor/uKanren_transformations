{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified Regexp1_simple
import qualified Regexp1_online
import qualified Regexp1_offline


eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval4 :: (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

eval5 :: (m r -> [r]) -> (a -> b -> c -> d -> e -> m r) -> (a, b, c, d, e) -> [r]
eval5 listify f = eval listify $ \(x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5

a = Zero
b = Succ a
c = Succ b
d = Succ c

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

main = defaultMain
  [
    bgroup "Regexp1"
     [
       bench "offline" $ nf (eval4 (takeS 20 ) Regexp1_offline.generatesdsIIIIO) (a, b, c, d)
     , bench "online"  $ nf (eval4 (takeS 20 ) Regexp1_online.generateIIIIO) (a, b, c, d)
     , bench "simple"  $ nf (eval5 (takeS 20 ) Regexp1_simple.help) (a, b, c, d, natGen)
     ]
  ]