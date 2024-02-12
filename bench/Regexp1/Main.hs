{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified Regexp1_unfold
import qualified Regexp1_cpd_ans
import qualified Regexp1_unfold1


eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval4 :: (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

a = Zero
b = Succ a
c = Succ b
d = Succ c

ans1 = (eval4 (takeS 20 ) Regexp1_unfold.generateIIIIO) (a, b, c, d)
ans2 = (eval4 (takeS 20 ) Regexp1_cpd_ans.generateIIIIO) (a, b, c, d)

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

main = defaultMain
  [
    bgroup "Regexp1"
     [
       bench "offlineIIIIO" $ nf (eval4 (takeS 20 ) Regexp1_unfold.generateIIIIO) (a, b, c, d) -- $ traceShow ans1 Regexp1_unfold.generateIIIIO) (a, b, c, d)
     , bench "offline1IIIIO"$ nf (eval4 (takeS 20 ) Regexp1_unfold1.generateIIIIO) (a, b, c, d) -- $ traceShow ans2 Regexp1_cpd_ans.generateIIIIO) (a, b, c, d)
     , bench "onlineIIIIO"  $ nf (eval4 (takeS 20 ) Regexp1_cpd_ans.generateIIIIO) (a, b, c, d) -- $ traceShow ans2 Regexp1_cpd_ans.generateIIIIO) (a, b, c, d)
     , bench "offlineOOIIO" $ nf (eval4 (takeS 20 ) Regexp1_unfold.generateOOIIO) (a, b, natGen, natGen)
     , bench "offline1OOIIO"$ nf (eval4 (takeS 20 ) Regexp1_unfold1.generateOOIIO) (a, b, natGen, natGen)
     , bench "onlineOOIIO"  $ nf (eval4 (takeS 20 ) Regexp1_cpd_ans.generateOOIIO) (a, b, natGen, natGen)
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