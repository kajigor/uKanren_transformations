{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow, trace)
import Term
import qualified Simple    
import qualified NthOpt_unfold
import qualified NthOpt_cpd_ans

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x,y,z) -> f x y z


eval4 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> m r) -> (x1, x2, x3, x4) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

eval5 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (x1, x2, x3, x4, x5) -> [r]
eval5 listify f = eval listify $ \(x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

rightList1 :: Term
rightList1 = Cons Zero (Cons (Succ Zero) Nil)

rightAns1 :: Term
rightAns1 = None

rightAns2 :: Term
rightAns2 = Some Zero

n :: Term
n = Succ (Succ (Succ Zero))

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)


ans1 = (eval5 (takeS 1) NthOpt_unfold.nthOptOI) (rightAns1, natGen, natGen, natGen, natGen)
ans2 = (eval4 (takeS 2) Simple.nthOptOII) (n, rightAns1, natGen, natGen)
ans3 = (eval4 (takeS 2) Simple.nthOptOII) (n, rightAns2, natGen, natGen)
ans4 = (eval3 (takeS 3) Simple.nthOptOIO) (n, natGen, natGen)

ans5 = (eval5 (takeS 1) NthOpt_cpd_ans.nthOptOI) (rightAns2, natGen, natGen, natGen, natGen)

main :: IO ()
main = defaultMain
  [
    bgroup "nthOpt"
     [ bench "offlineII"    $ nf (eval2 (takeS 1) NthOpt_unfold.nthOptII) (rightList1, rightAns1)
     , bench "onlineII"     $ nf (eval2 (takeS 1) NthOpt_cpd_ans.nthOptII) (rightList1, rightAns1)
     , bench "simpleII"     $ nf (eval3 (takeS 1) Simple.nthOptIII)      (rightList1, n, rightAns1)
     , bench "offlineIO"    $ nf (eval (takeS 1)  NthOpt_unfold.nthOptIO) rightList1
     , bench "onlineIO"     $ nf (eval (takeS 1) NthOpt_cpd_ans.nthOptIO) rightList1
     , bench "simpleIO"     $ nf (eval2 (takeS 1) Simple.nthOptIIO)      (rightList1, n)
     -- nthOptOIOffline, nthOptOIOnline
     , bench "offlineOI1"    $ nf (eval5 (takeS 1) NthOpt_unfold.nthOptOI) $ traceShow ans1 (rightAns1, natGen, natGen, natGen, natGen)
     , bench "onlineOI1"     $ nf (eval5 (takeS 1) NthOpt_unfold.nthOptOI) (rightAns1, natGen, natGen, natGen, natGen) 
     , bench "simpleOI1"     $ nf (eval4 (takeS 1) Simple.nthOptOII)       $ traceShow ans2 (n, rightAns1, natGen, natGen)
     , bench "offlineOI2"    $ nf (eval5 (takeS 1) NthOpt_unfold.nthOptOI) (rightAns2, listGen, listGen, natGen, listGen) -- fixed ?? 
     , bench "onlineOI2"     $ nf (eval5 (takeS 1) NthOpt_cpd_ans.nthOptOI)(rightAns2, natGen, natGen, natGen, natGen)
     , bench "simpleOI2"     $ nf (eval4 (takeS 1) Simple.nthOptOII)       $ traceShow ans3 (n, rightAns2, natGen, natGen)
     -- nthOptOOOffline, nthOptOOOnline 5
     , bench "offlineOO"     $ nf (eval4 (takeS 1) NthOpt_unfold.nthOptOO) (natGen, natGen, natGen, natGen)
     , bench "onlineOO"      $ nf (eval5 (takeS 1) NthOpt_cpd_ans.nthOptOO) (natGen, natGen, natGen, natGen, natGen) 
     , bench "simpleOO"      $ nf (eval3 (takeS 1) Simple.nthOptOIO)       $ traceShow ans4 (n, natGen, natGen)
     ]
  ]

--main =
--  print $ (takeS 1) $ NthOpt_unfold.nthOptOI rightAns2 listGen listGen natGen listGen