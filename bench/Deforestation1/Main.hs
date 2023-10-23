{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)


import Simple (Term (Cons, O, S, Nil) , rrIISimple)
    
rrIOff x0 = msum [do {(x1, x3) <- case x0 of
                               {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                   x2 <- case x1 of
                         {S y2 -> return y2; _ -> mzero};
                   guard (x2 == O);
                   guard (x3 == Nil);
                   return ()}]
                   

rrIOnline x0 = msum [do {rRI x0; return ()}]
rRI x0 = msum [do {rI x0; return ()}]
rI x0 = msum [do {(x1, x3) <- case x0 of
                              {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                  x2 <- case x1 of
                        {S y2 -> return y2; _ -> mzero};
                  guard (x2 == O);
                  guard (x3 == Nil);
                  return ()}]
                   
eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

rightTerm = Cons (S O) Nil
wrongTerm = Cons (S O) (Cons O (Cons (S O) Nil))

firstTerm = Cons (S O) (Cons (S O) (Cons (S O) (Cons (S O) Nil)))

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval14 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) -> [r]
eval14 listify f = eval listify $ \(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) -> f y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

put14 x y = 
  (x, y, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)

main :: IO ()
main = defaultMain
  [
    bgroup "Deforestation1"
     [ bench "offline1"    $ nf (eval (takeS 1) rrIOff) wrongTerm
     , bench "online1"     $ nf (eval (takeS 1) rrIOnline) wrongTerm
--     , bench "simple1"     $ nf (eval14 (takeS 1) rrIISimple) $ put14 firstTerm wrongTerm -- failing
     , bench "offline2"    $ nf (eval (takeS 1) rrIOff) rightTerm
     , bench "online2"     $ nf (eval (takeS 1) rrIOnline) rightTerm
--     , bench "simple2"     $ nf (eval14 (takeS 1) rrIISimple) $ put14 firstTerm rightTerm -- failing
     ]
  ]