import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified Sort_cpd_ans
import qualified Sort_unfold
import qualified Simple

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval5 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4-> m r) -> (a, x1, x2, x3, x4) -> [r]
eval5 listify f = eval listify  $ \(b, y1, y2, y3, y4) -> f b y1 y2 y3 y4

eval6 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (a, x1, x2, x3, x4, x5) -> [r]
eval6 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5) -> f b y1 y2 y3 y4 y5

eval7 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> m r) -> (x1, x2, x3, x4, x5, x6, x7) -> [r]
eval7 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7) -> f y1 y2 y3 y4 y5 y6 y7

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

--[Zero, Zero, Succ Zero, Succ Zero, Succ (Succ Zero)]
rightSort :: Term
rightSort = Cons Zero (Cons Zero (Cons (Succ Zero) (Cons (Succ Zero) (Cons (Succ (Succ Zero)) Nil))))

rightGen :: (Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term)
rightGen = (rightSort, natGen, natGen, natGen, natGen, natGen, natGen)

genSort :: (Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term)
genSort = (natGen, natGen, natGen, natGen, natGen, natGen, natGen)

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f
  
-- [Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero] 
input = Cons (Succ Zero) (Cons Zero (Cons (Succ (Succ Zero)) (Cons (Succ Zero) (Cons Zero Nil))))

main = defaultMain
  [
    bgroup "SortRun"
     [
        bench "offlineI"    $ nf (eval (takeS 1) Sort_unfold.sortoI) rightSort
      , bench "onlineI"     $ nf (eval (takeS 1) Sort_cpd_ans.sortoI) rightSort
      , bench "simpleI"     $ nf (eval2 (takeS 1) Simple.sortoII) (input, rightSort)
     ],
    bgroup "SortGen"
    [
        bench "offlineO"    $ nf (eval0 (takeS 1) Sort_unfold.sortoO) ()
      , bench "onlineO"     $ nf (eval0 (takeS 1) Sort_cpd_ans.sortoO) ()
      , bench "simpleO"     $ nf (eval (takeS 1) Simple.sortoIO) input
    ]
  ]
--
--main =
--   print $ (takeS 1) $ Simple.sortoII input rightSort 

  
