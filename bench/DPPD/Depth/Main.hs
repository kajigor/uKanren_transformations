{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import Term
import qualified Depth_offline
import qualified Depth_online
import qualified Depth_simple

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x,y,z) -> f x y z

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)
  
getNat :: Int -> Term 
getNat x | x == 0 = O
getNat x | x > 0 = 
  S $ getNat (x - 1)
  
n0 = getNat 0 
n1 = getNat 1
n2 = getNat 2 
n3 = getNat 3

input = Member n0 (Cons n1 (Cons n2 (Cons n2 (Cons n3 (Cons n1 (Cons n0 (Cons n2 (Cons n0 (Cons n1 (Cons n2 (Cons n0 (Cons n0 Nil)))))))))))) 

main = defaultMain
  [
    bgroup "DepthGen"
    [
        bench "offlineGen"  $ nf (eval0 (takeS 3) Depth_offline.depthsdO) () 
      , bench "onlineGen"   $ nf (eval0 (takeS 3) Depth_online.depthO) () 
      , bench "simpleGen"   $ nf (eval3 (takeS 3) Depth_simple.depthIO) (input, listGen, listGen) -- failing
    ] 
  ]
