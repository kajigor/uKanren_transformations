{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified List3_cpd_ans
import qualified List3_unfold

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: MonadPlus m => (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: MonadPlus m => (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x1, x2) -> f x1 x2

eval3 :: MonadPlus m =>  (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x1, x2, x3) -> f x1 x2 x3

eval4 :: MonadPlus m =>  (m r -> [r]) -> (a -> b -> c -> d-> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

eval6 :: MonadPlus m => Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (a, x1, x2, x3, x4, x5) -> [r]
eval6 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5) -> f b y1 y2 y3 y4 y5

eval7 :: MonadPlus m => Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> m r) -> (a, x1, x2, x3, x4, x5, x6) -> [r]
eval7 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6) -> f b y1 y2 y3 y4 y5 y6

--  [Succ (Succ Zero), Zero, Succ Zero, Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero, Zero, Succ (Succ Zero)]

ts = Cons (Succ (Succ Zero)) (Cons Zero (Cons (Succ Zero) (Cons (Succ Zero) (Cons Zero (Cons (Succ (Succ Zero)) (Cons (Succ Zero) (Cons Zero (Cons Zero (Cons (Succ (Succ Zero)) Nil)))))))))
zs = Cons (Succ (Succ Zero)) (Cons (Zero) (Cons (Zero) (Cons (Succ Zero) (Cons (Succ (Succ Zero)) (Cons (Zero) (Cons (Succ Zero) (Cons (Succ Zero) (Cons (Zero) (Cons (Succ (Succ Zero)) Nil)))))))))
xs = Cons (Succ (Succ Zero)) (Cons (Zero) (Cons (Zero) (Cons (Succ Zero) (Cons (Succ (Succ Zero)) (Cons (Zero) Nil)))))
ys = Cons (Succ Zero) (Cons (Succ Zero) (Cons (Zero) (Cons (Succ (Succ Zero)) Nil)))

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

main = defaultMain
  [
    bgroup "append + revers"
     [
        bench "offlineIII"    $ nf (eval3 (takeS 1) List3_unfold.helpIII) (xs, ys, zs)
      , bench "onlineIII"     $ nf (eval3 (takeS 1) List3_cpd_ans.helpIII) (xs, ys, zs)
      , bench "offlineIIO"    $ nf (eval2 (takeS 1) List3_unfold.helpIIO) (xs, ys)
      , bench "onlineIIO"     $ nf (eval3 (takeS 1) List3_cpd_ans.helpIIO) (xs, ys, listGen)
      , bench "offlineOOI"    $ nf (eval2 (takeS 10) List3_unfold.helpOOI) (zs, listGen) --
      , bench "onlineOOI"     $ nf (eval2 (takeS 10) List3_cpd_ans.helpOOI) (zs, listGen)
      , bench "offlineOOO"    $ nf (eval (takeS 10) List3_unfold.helpOOO) (listGen)
      , bench "onlineOOO"     $ nf (eval2 (takeS 10) List3_cpd_ans.helpOOO) (listGen, listGen)

     ]
--    ,bgroup "SortGen"
--    [
--        bench "offlineGen"  $ nf (eval26 (takeS 1) double_appendoIIIOnline) $ natGen23 a b c
--      , bench "onlineGen"   $ nf (eval7 (takeS 1) sortoOOnline) genSort -- failing
--    ]
  ]


--main =
--  print $ (takeS 5) $ DoubleAppend_cpd_ans.double_appendoOOO listGen listGen listGen listGen listGen listGen

