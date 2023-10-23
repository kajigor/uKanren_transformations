{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import Simple (Term (Cons, Zero, Succ, Nil) , applastoIIISimple)

applastoIOff x0 = msum [do {(x1, x2) <- case x0 of
                                     {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                         appendoLastoIIOff x1 x2;
                         return ()}]
appendoLastoIIOff x0 x1 = msum [do {(x2, x3) <- case x1 of
                                             {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                 appendoLastoIIOff x2 x3;
                                 return ()}]
applastoOOff gen_appendoLastoOO_x0 = msum [do {(x1,
                                             x2) <- appendoLastoOOOff gen_appendoLastoOO_x0;
                                            let {x0 = Cons x1 x2};
                                            return x0}]
appendoLastoOOOff gen_appendoLastoOO_x0 = msum [do {(x2,
                                                  x3) <- appendoLastoOOOff gen_appendoLastoOO_x0;
                                                 let {x1 = Cons x2 x3};
                                                 x0 <- gen_appendoLastoOO_x0;
                                                 return (x0, x1)}]
                                               
applastoIOnl x0 = msum [do {(x1, x2) <- case x0 of
                                     {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                         appendoLastoIIOnl x1 x2;
                         return ()}]
appendoLastoIIOnl x0 x1 = msum [do {(x2, x3) <- case x1 of
                                             {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                 appendoLastoIIOnl x2 x3;
                                 return ()}]
applastoOOnl gen_appendoLastoOO_x0 = msum [do {(x1,
                                             x2) <- appendoLastoOOOnl gen_appendoLastoOO_x0;
                                            let {x0 = Cons x1 x2};
                                            return x0}]
appendoLastoOOOnl gen_appendoLastoOO_x0 = msum [do {(x2,
                                                  x3) <- appendoLastoOOOnl gen_appendoLastoOO_x0;
                                                 let {x1 = Cons x2 x3};
                                                 x0 <- gen_appendoLastoOO_x0;
                                                 return (x0, x1)}]          

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval4 :: (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

eval6N :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> m r) -> (a, x1, x2, x3, x4, x5, x6) -> [r]
eval6N listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6) -> f b y1 y2 y3 y4 y5 y6

eval7 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> m r) -> (x1, x2, x3, x4, x5, x6, x7) -> [r]
eval7 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7) -> f y1 y2 y3 y4 y5 y6 y7

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

listExample :: Term
listExample = Cons Zero (Cons (Succ Zero) Nil)

main = defaultMain
  [
    bgroup "ApplastRun"
     [
        bench "offline1"    $ nf (eval (takeS 1) applastoIOff) listExample
      , bench "online1"     $ nf (eval (takeS 1) applastoIOnl) listExample 
      , bench "simple1"   $ nf (eval4 (takeS 1) applastoIIISimple) (listExample, Zero, Succ Zero, natGen)
     ],
    bgroup "ApplastGen"
    [
       -- bench "offlineGen"  $ nf (eval (takeS 1) applastoOOff) natGen -- failing
       --, bench "onlineGen"   $ nf (eval (takeS 1) applastoOOnl) natGen -- failing
    ]
  ]