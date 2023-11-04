{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified Remove2_unfold
import qualified Remove2_cpd_ans

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x1, x2) -> f x1 x2

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x1, x2, x3) -> f x1 x2 x3

eval10 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) -> [r]
eval10 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) -> f y1 y2 y3 y4 y5 y6 y7 y8 y9 y10

eval12 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) -> [r]
eval12 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12) -> f y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12

eval14 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) -> [r]
eval14 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) -> f y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14

eval26 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24 -> x25 -> m r) -> (a, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25) -> [r]
eval26 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25) -> f b y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25

eval44 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24 -> x25 -> x26 -> x27 -> x28 -> x29 -> x30 -> x31 -> x32 -> x33 -> x34 -> x35 -> x36 -> x37 -> x38 -> x39 -> x40 -> x41 -> x42 -> x43 -> m r) -> (a, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43) -> [r]
eval44 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31, y32, y33, y34, y35, y36, y37, y38, y39, y40, y41, y42, y43) -> f b y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25 y26 y27 y28 y29 y30 y31 y32 y33 y34 y35 y36 y37 y38 y39 y40 y41 y42 y43

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

rightTerm :: Term
rightTerm = Cons (Succ Zero) (Cons (Succ Zero) (Cons (Succ Zero) (Cons (Succ Zero) (Cons Zero (Cons Zero (Cons Zero (Cons Zero (Cons (Succ (Succ Zero)) (Cons (Succ (Succ Zero)) (Cons (Succ (Succ Zero)) (Cons (Succ (Succ Zero)) (Cons Zero (Cons Zero (Cons Zero (Cons Zero (Cons Zero (Cons Zero (Cons Zero (Cons Zero Nil)))))))))))))))))))

wrongTerm :: Term
wrongTerm = Cons (Succ Zero) Nil

rightGen14 :: MonadPlus m => (Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term)
rightGen14 = (rightTerm, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)

rightGen12 :: MonadPlus m => (Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term, m Term)
rightGen12 = (rightTerm, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)

main = defaultMain
  [
    bgroup "Remove2"
     [
--       bench "onlineII" $ nf (eval12 (takeS 1 ) Remove2_cpd_ans.rrI) rightGen12
--        bench "offlineII" $ nf (eval14 (takeS 1 ) Remove2_unfold.rrI) rightGen14
--      , bench "onlineII" $ nf (eval12 (takeS 1 ) Remove2_cpd_ans.rrI) rightGen12
--      , bench "offlineOI" $ nf (eval (takeS 1 ) Ex_Depth_unfold.solveOI) y1
--      , bench "onlineII"  $ nf (eval44 (takeS 1) Ex_Depth_cpd_ans.solveII) (y0, y1, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)
     ]
--    ,bgroup "SortGen"
--    [
--        bench "offlineGen"  $ nf (eval26 (takeS 1) double_appendoIIIOnline) $ natGen23 a b c
--      , bench "onlineGen"   $ nf (eval7 (takeS 1) sortoOOnline) genSort -- failing
--    ]
  ]