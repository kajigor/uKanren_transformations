{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified Deforestation2_offline
import qualified Deforestation2_online
import qualified Deforestation2_simple
import Term

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x,y,z) -> f x y z

--[S O, O, S (S O), O, O]
secondTerm = Cons (S O) (Cons O (Cons (S (S O)) (Cons O (Cons O Nil))))

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

main :: IO ()
main = defaultMain
  [
    bgroup "Deforestation2"
     [
       bench "offlineO"   $ nf (eval0 (takeS 10) Deforestation2_offline.rrdsO) ()  -- failing
     , bench "onlineO"    $ nf (eval3 (takeS 10) Deforestation2_online.rrO) (natGen, natGen, listGen)
     , bench "simpleO"    $ nf (eval2 (takeS 10) Deforestation2_simple.rrOI) (secondTerm, natGen)
     ]
  ]
