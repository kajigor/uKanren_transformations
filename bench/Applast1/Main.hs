{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import NewVersion (applastoI, applastoO)
import Term
import Simple (applastoIIISimple, applastoOOOSimple)

applastoIOnl x0 = msum [do {guard (x0 == Nil); return ()},
                     do {(x1, x2) <- case x0 of
                                     {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                         appendoLastoIIOnl x1 x2;
                         return ()}]
appendoLastoIIOnl x0 x1 = msum [do {guard (x1 == Nil); return ()},
                             do {(x2, x3) <- case x1 of
                                             {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                 appendoLastoIIOnl x2 x3;
                                 return ()}]
applastoOOnl gen_appendoLastoOO_x0 = msum [do {let {x0 = Nil};
                                            return x0},
                                        do {(x1, x2) <- appendoLastoOOOnl gen_appendoLastoOO_x0;
                                            let {x0 = Cons x1 x2};
                                            return x0}]
appendoLastoOOOnl gen_appendoLastoOO_x0 = msum [do {let {x1 = Nil};
                                                 x0 <- gen_appendoLastoOO_x0;
                                                 return (x0, x1)},
                                             do {(x2, x3) <- appendoLastoOOOnl gen_appendoLastoOO_x0;
                                                 let {x1 = Cons x2 x3};
                                                 x0 <- gen_appendoLastoOO_x0;
                                                 return (x0, x1)}]

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval4 :: (m r -> [r]) -> (a -> b -> c -> d -> m r) -> (a, b, c, d) -> [r]
eval4 listify f = eval listify $ \(x1, x2, x3, x4) -> f x1 x2 x3 x4

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

listExample :: Term
listExample = Cons Zero (Cons (Succ Zero) Nil)

main = defaultMain
  [
    bgroup "ApplastRun"
     [
        bench "online1"     $ nf (eval (takeS 1) applastoIOnl) listExample
      , bench "simple1"   $ nf (eval4 (takeS 1) applastoIIISimple) (listExample, Zero, Succ Zero, natGen)
      , bench "newVersion1" $ nf (eval (takeS 1) applastoI) listExample
     ],
    bgroup "ApplastGen"
    [
        bench "onlineGen"   $ nf (eval (takeS 1) applastoOOnl) natGen -- failing
       , bench "simple1"   $ nf (eval4 (takeS 1) applastoOOOSimple) (listGen, natGen, natGen, natGen)
       , bench "newVersionO"  $ nf (eval2 (takeS 1) applastoO) (natGen, natGen)
    ]
  ]

--main =
--  print $ (takeS 1) $ applastoOOff natGen