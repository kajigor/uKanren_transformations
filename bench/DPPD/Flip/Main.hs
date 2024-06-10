{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import Term
import qualified Flip_offline
import qualified Flip_online
import qualified Flip_simple

eval0 :: (m r -> [r]) -> (m r) -> a -> [r]
eval0 listify f x = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

tree = (Tree (Tree (Tree (Tree (Leaf (A)) (E) (Leaf (B))) (D) (Leaf (C))) (E) (Leaf (B))) (D) (Tree (Leaf (F)) (R) (Leaf (C))))

main = defaultMain
  [
    bgroup "Flip"
     [
        bench "offlineO" $ nf (eval0 (takeS 1) Flip_offline.flipflipdsO) ()
      , bench "onlineO"  $ nf (eval0 (takeS 1) Flip_online.flipflipO) ()
      , bench "simpleO"  $ nf (eval  (takeS 1) Flip_simple.flipflipOI) (tree) 
     ]
  ]


