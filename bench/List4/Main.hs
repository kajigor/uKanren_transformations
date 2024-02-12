{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)

import Simple (Term (..), maxMinoIII, maxMinoIOO)
   
maxMinoIIOnline x0 x1 = msum [do {maxo1I x0; mino1I x1; return ()}]
maxMinoIOOnline x0 = msum [do {maxo1I x0; x1 <- mino1O; return x1}]
maxMinoOIOnline x1 = msum [do {x0 <- maxo1O; mino1I x1; return x0}]
maxMinoOOOnline = msum [do {x0 <- maxo1O; x1 <- mino1O; return (x0, x1)}]
maxo1I x0 = msum [do {maxo1_1I x0; return ()}]
maxo1O = msum [do {x0 <- maxo1_1O; return x0}]
maxo1_1I x0 = msum [do {x2 <- case x0 of
                              {Succ y2 -> return y2; _ -> mzero};
                        x3 <- case x2 of
                              {Succ y3 -> return y3; _ -> mzero};
                        guard (x3 == Zero);
                        return ()}]
maxo1_1O = msum [do {let {x3 = Zero};
                     let {x2 = Succ x3};
                     let {x0 = Succ x2};
                     return x0}]
mino1I x0 = msum [do {mino1_1I x0; return ()}]
mino1O = msum [do {x0 <- mino1_1O; return x0}]
mino1_1I x0 = msum [do {guard (x0 == Zero); return ()}]
mino1_1O = msum [do {let {x0 = Zero}; return x0}]


maxMinoIIOffline x0 x1 = msum [do {let {x3 = Zero};
                            let {x2 = Succ x3};
                            guard (x1 == Zero);
                            x4 <- case x0 of
                                  {Succ y4 -> return y4; _ -> mzero};
                            guard (x4 == x2);
                            return ()}]
maxMinoIOOffline x0 = msum [do {let {x1 = Zero};
                         let {x3 = Zero};
                         let {x2 = Succ x3};
                         x4 <- case x0 of
                               {Succ y4 -> return y4; _ -> mzero};
                         guard (x4 == x2);
                         return x1}]
maxMinoOIOffline x1 = msum [do {let {x3 = Zero};
                         let {x2 = Succ x3};
                         guard (x1 == Zero);
                         let {x4 = x2};
                         let {x0 = Succ x4};
                         return x0}]
maxMinoOOOffline = msum [do {let {x1 = Zero};
                      let {x3 = Zero};
                      let {x2 = Succ x3};
                      let {x4 = x2};
                      let {x0 = Succ x4};
                      return (x0, x1)}]
eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval5 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (x1, x2, x3, x4, x5) -> [r]
eval5 listify f = eval listify $ \(x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

fstTerm = Cons (Succ (Succ Zero)) (Cons Zero (Cons (Succ Zero) Nil))

main :: IO ()
main = defaultMain
  [
    bgroup "List4"
     [ bench "offlineII"    $ nf (eval2 (takeS 1) maxMinoIIOffline) (Succ (Succ Zero), Zero)
     , bench "onlineII"     $ nf (eval2 (takeS 1) maxMinoIIOnline) (Succ (Succ Zero), Zero)
     , bench "simpleII"     $ nf (eval5 (takeS 1) maxMinoIII) (fstTerm, Succ (Succ Zero), Zero, natGen, natGen)
     , bench "offlineOO"    $ nf (eval0 (takeS 1) maxMinoOOOffline) () 
     , bench "onlineOO"     $ nf (eval0 (takeS 1) maxMinoOOOnline) ()
     , bench "simpleOO"     $ nf (eval (takeS 1) maxMinoIOO) fstTerm
     ]
  ]

--main =
--  print $ (takeS 1) $ maxMinoIII fstTerm (Succ (Succ Zero)) Zero natGen natGen

--  [S O, S O, S O, S O, O, O, O, O, S (S O), S (S O), S (S O), S (S O), O, O, O, O, O, O, O, O]

--  Cons (S O) (Cons (S O) (Cons (S O) (Cons (S O) (Cons O (Cons O (Cons O (Cons O (Cons (S (S O)) (Cons (S (S O)) (Cons (S (S O)) (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O Nil))))))))))))))))))