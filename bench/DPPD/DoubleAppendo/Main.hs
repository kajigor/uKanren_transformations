{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified DoubleAppend_offline
import qualified DoubleAppend_online
import qualified DoubleAppend_simple

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

a = Cons (S O) (Cons (S (S O)) Nil)
b = Cons (S (S (S O))) (Cons O (Cons O Nil))
c = Cons (S O) (Cons (S (S O)) Nil)
d = Cons (S O) (Cons (S (S O)) (Cons (S (S (S O))) (Cons O (Cons O (Cons (S O) (Cons (S (S O)) Nil))))))

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

main = defaultMain
  [
    bgroup "DoubleAppend"
     [
        bench "offlineOOO"    $ nf (eval0 (takeS 60) DoubleAppend_offline.double_appendodddsOOO) ()
      , bench "onlineOOO"     $ nf (eval6 (takeS 60) DoubleAppend_online.double_appendoOOO) (listGen, listGen, listGen, listGen, listGen, listGen)
      , bench "simpleOOO"     $ nf (eval (takeS 60) DoubleAppend_simple.double_appendoOOOI) d

     ]
--    ,bgroup "SortGen"
--    [
--        bench "offlineGen"  $ nf (eval26 (takeS 1) double_appendoIIIOnline) $ natGen23 a b c
--      , bench "onlineGen"   $ nf (eval7 (takeS 1) sortoOOnline) genSort -- failing
--    ]
  ]


--main =
--  print $ (takeS 5) $ DoubleAppend_cpd_ans.double_appendoOOO listGen listGen listGen listGen listGen listGen

