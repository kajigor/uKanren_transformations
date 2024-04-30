
import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import Term
import qualified RotatePrune_offline
import qualified RotatePrune_online
import qualified RotatePrune_simple

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x1, x2) -> f x1 x2

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x1, x2, x3) -> f x1 x2 x3

eval4 :: (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

-- natGen :: (MonadPlus m) => m Term
natGen = Immature $ msum [do {
    return O
  }, do {
    n <- natGen;
    return (S n)
  }]

-- genTree :: (MonadPlus m) => m Term
genTree = Immature $ msum [do {
    n <- natGen;
    return (Leaf n)
  }, do {
    n <- natGen;
    l <- genTree;
    r <- genTree;
    return (Tree l n r)
  } 
  ]

tree = (Tree (Tree (Leaf O) (S O) (Leaf (S O)) ) (S O) (Leaf O))

main = defaultMain
  [
    bgroup "RotatePrune"
     [
        bench "offlineO" $ nf (eval4 (takeS 100) RotatePrune_offline.pruneO) (genTree, genTree, genTree, genTree)
      , bench "onlineO"  $ nf (eval3 (takeS 100) RotatePrune_online.pruneO) (genTree, genTree, genTree)
      , bench "simpleO"  $ nf (eval3 (takeS 100) RotatePrune_simple.prune) (tree, genTree, genTree)
     ]
  ]