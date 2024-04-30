{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified Applast1_online
import qualified Applast1_simple
import qualified Applast1_offline
import Term

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
        bench "online"     $ nf (eval (takeS 1) Applast1_online.applastoI) listExample
      , bench "simple"     $ nf (eval4 (takeS 1) Applast1_simple.applastoIII) (listExample, Succ Zero, Succ Zero, natGen)
      , bench "offline"    $ nf (eval (takeS 1) Applast1_offline.applastoI) listExample
     ],
    bgroup "ApplastGen"
    [
        bench "online"     $ nf (eval (takeS 1) Applast1_online.applastoO) natGen -- failing
      , bench "simple"     $ nf (eval4 (takeS 1) Applast1_simple.applastoOOO) (listGen, natGen, natGen, natGen)
      , bench "offline"    $ nf (eval2 (takeS 1) Applast1_offline.applastoO) (natGen, natGen)
    ]
  ]

--main =
--  print $ (takeS 1) $ applastoOOff natGen