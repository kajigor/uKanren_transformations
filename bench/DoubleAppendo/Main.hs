{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import DoubleAppend_unfold (double_appendoIIIOffline)
import DoubleAppend_cpd_ans (double_appendoIIIOnline)

-- double_appendoIIIOnline 26
-- double_appendoIIIOffline 26

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval26 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24 -> x25 -> m r) -> (a, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25) -> [r]
eval26 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25) -> f b y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

natGen23 x1 x2 x3 =
  (x1, x2, x3, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen,
  natGen, natGen, natGen, natGen, natGen, natGen)

a = Cons (S O) (Cons (S (S O)) Nil)
b = Cons (S (S (S O))) (Cons O (Cons O Nil))
c = Cons (S O) (Cons (S (S O)) Nil)

main = defaultMain
  [
    bgroup "SortRun"
     [
        bench "offline1"    $ nf (eval26 (takeS 1) double_appendoIIIOffline) $ natGen23 a b c
--      , bench "online1"     $ nf (eval6N (takeS 1) sortoIOnline) $ traceShow (eval6N (takeS 1) sortoIOnline rightGen) rightGen -- failing
     ],
    bgroup "SortGen"
    [
        bench "offlineGen"  $ nf (eval26 (takeS 1) double_appendoIIIOnline) $ natGen23 a b c
   --   , bench "onlineGen"   $ nf (eval7 (takeS 1) sortoOOnline) genSort -- failing
    ]
  ]


