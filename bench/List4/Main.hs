{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)

import Simple (Term (..), maxMinoIII)
   
maxMinoIIOnline x0 x1 = msum [do {maxo1I x0; mino1I x1; return ()}]
--maxMinoIO x0 = msum [do {maxo1I x0; x1 <- mino1O; return x1}]
--maxMinoOI x1 = msum [do {x0 <- maxo1O; mino1I x1; return x0}]
--maxMinoOO = msum [do {x0 <- maxo1O; x1 <- mino1O; return (x0, x1)}]
maxo1I x0 = msum [do {maxo1_1I x0; return ()}]
--maxo1O = msum [do {x0 <- maxo1_1O; return x0}]
maxo1_1I x0 = msum [do {x2 <- case x0 of
                              {Succ y2 -> return y2; _ -> mzero};
                        x3 <- case x2 of
                              {Succ y3 -> return y3; _ -> mzero};
                        guard (x3 == Zero);
                        return ()}]
--maxo1_1O = msum [do {let {x3 = Zero};
--                     let {x2 = Succ x3};
--                     let {x0 = Succ x2};
--                     return x0}]
mino1I x0 = msum [do {mino1_1I x0; return ()}]
--mino1O = msum [do {x0 <- mino1_1O; return x0}]
mino1_1I x0 = msum [do {guard (x0 == Zero); return ()}]
--mino1_1O = msum [do {let {x0 = Zero}; return x0}]


maxMinoIIOffline x0 x1 = msum [do {guard (x1 == Zero);
                            x2 <- case x0 of
                                  {Succ y2 -> return y2; _ -> mzero};
                            x3 <- case x2 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            guard (x3 == Zero);
                            return ()}]
--maxMinoIO x0 = msum [do {let {x1 = Zero};
--                         x2 <- case x0 of
--                               {Succ y2 -> return y2; _ -> mzero};
--                         x3 <- case x2 of
--                               {Succ y3 -> return y3; _ -> mzero};
--                         guard (x3 == Zero);
--                         return x1}]
--maxMinoOI x1 = msum [do {guard (x1 == Zero);
--                         let {x3 = Zero};
--                         let {x2 = Succ x3};
--                         let {x0 = Succ x2};
--                         return x0}]
--maxMinoOO = msum [do {let {x1 = Zero};
--                      let {x3 = Zero};
--                      let {x2 = Succ x3};
--                      let {x0 = Succ x2};
--                      return (x0, x1)}]


eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval5 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (x1, x2, x3, x4, x5) -> [r]
eval5 listify f = eval listify $ \(x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

fstTerm = Cons (Succ Zero) (Cons Zero (Cons (Succ Zero) Nil)) 

main :: IO ()
main = defaultMain
  [
    bgroup "List4"
     [ bench "offline1"    $ nf (eval2 (takeS 1) maxMinoIIOffline) (Succ Zero, Succ Zero)
     , bench "online1"     $ nf (eval2 (takeS 1) maxMinoIIOnline) (Succ Zero, Succ Zero)
--     , bench "simple1"     $ nf (eval5 (takeS 1) maxMinoIII) (fstTerm, Succ Zero, Succ Zero, natGen, natGen) --failing
     , bench "offline2"    $ nf (eval2 (takeS 1) maxMinoIIOffline) (Succ (Succ Zero), Zero)
     , bench "online2"     $ nf (eval2 (takeS 1) maxMinoIIOnline) (Succ (Succ Zero), Zero)
--     , bench "simple2"     $ nf (eval5 (takeS 1) maxMinoIII) (fstTerm, Succ (Succ Zero), Succ Zero, natGen, natGen) --failing
     ]
  ]

--  [S O, S O, S O, S O, O, O, O, O, S (S O), S (S O), S (S O), S (S O), O, O, O, O, O, O, O, O]

--  Cons (S O) (Cons (S O) (Cons (S O) (Cons (S O) (Cons O (Cons O (Cons O (Cons O (Cons (S (S O)) (Cons (S (S O)) (Cons (S (S O)) (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O Nil))))))))))))))))))