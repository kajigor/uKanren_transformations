{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified MatchSimple_online
import qualified MatchSimple_offline
import qualified MatchSimple_simple

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: MonadPlus m => (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval4 :: MonadPlus m => (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

eval50 :: MonadPlus m => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24 -> x25 -> x26 -> x27 -> x28 -> x29 -> x30 -> x31 -> x32 -> x33 -> x34 -> x35 -> x36 -> x37 -> x38 -> x39 -> x40 -> x41 -> x42 -> x43 -> x44 -> x45 -> x46 -> x47 -> x48 -> x49 -> m r) -> (a, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49) -> [r]
eval50 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31, y32, y33, y34, y35, y36, y37, y38, y39, y40, y41, y42, y43, y44, y45, y46, y47, y48, y49) -> f b y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25 y26 y27 y28 y29 y30 y31 y32 y33 y34 y35 y36 y37 y38 y39 y40 y41 y42 y43 y44 y45 y46 y47 y48 y49  

pt = Cons (S (S O)) (Cons (S O) (Cons (S O) (Cons O (Cons O (Cons (S (S O)) (Cons O (Cons (S O) Nil)))))))

termGen = natGen <|> listGen

main = defaultMain
  [
    bgroup "matchSimple"
     [
        bench "offlineO45"    $ nf (eval0 (takeS 45) MatchSimple_offline.matchdsO) ()
      , bench "onlineO45"     $ nf (eval50 (takeS 45) MatchSimple_online.matchO) (termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen,
      termGen, termGen, termGen, termGen, termGen)
      , bench "simple045"     $ nf (eval4 (takeS 45) MatchSimple_simple.match) (pt, listGen, listGen, natGen)
     ]
  ]
