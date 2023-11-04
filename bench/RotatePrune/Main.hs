
import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified RotatePrune_unfold
import qualified RotatePrune_cpd_ans

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x1, x2) -> f x1 x2

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x1, x2, x3) -> f x1 x2 x3

eval4 :: (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

main = defaultMain
  [
    bgroup "RotatePrune"
     [
       bench "offlineO" $ nf (eval4 (takeS 1 ) RotatePrune_unfold.pruneO) (natGen, natGen, natGen, natGen)
     , bench "onlineO"  $ nf (eval3 (takeS 1 ) RotatePrune_cpd_ans.pruneO) (natGen, natGen, natGen)
     ]
  ]