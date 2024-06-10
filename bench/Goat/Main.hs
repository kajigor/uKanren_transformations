{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)
import Term

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified Goat_unfold
import qualified Goat_online
import qualified Goat_conspd
import qualified Goat_simple

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f


start :: Term
start = Pair (Quad Term.True Term.True Term.True Term.True) (Quad Term.False Term.False Term.False Term.False)

end :: Term
end = Pair (Quad Term.False Term.False Term.False Term.False) (Quad Term.True Term.True Term.True Term.True)

eval4 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> m r) -> (x1, x2, x3, x4) -> [r]
eval4 listify f = eval listify  $ \(y1, y2, y3, y4) -> f y1 y2 y3 y4 

--Cabbage
--    | Cons Term Term
--    | Empty
--    | False
--    | Goat
--    | Man
--    | Nil
--    | Pair Term Term
--    | Quad Term Term Term Term
--    | True
--    | Wolf


boolGen :: (MonadPlus m) => m Term
boolGen = (return Term.True) <|> (return Term.False)

confGen :: (MonadPlus m) => m Term
confGen = do
  x1 <- boolGen
  x2 <- boolGen
  x3 <- boolGen
  x4 <- boolGen
  return (Quad x1 x2 x3 x4)

ans = (takeS 5) Goat_unfold.evalO
ans1 = (takeS 5) Goat_online.evalO

main = defaultMain
  [
    bgroup "Goat"
     [
        bench "offline5"   $ nf (eval0 (takeS 5) $ traceShow ans Goat_unfold.evalO) ()
      , bench "online5"    $ nf (eval0 (takeS 5) $ traceShow ans1 Goat_online.evalO) ()
      , bench "simple5"    $ nf (eval4 (takeS 5) Goat_simple.evalIOI) (start, end, confGen, confGen)
      , bench "offlineO"   $ nf (eval0 (takeS 50) $ traceShow ans Goat_unfold.evalO) ()
      , bench "onlineO"    $ nf (eval0 (takeS 50) $ traceShow ans1 Goat_online.evalO) ()
      , bench "offlineO100"   $ nf (eval0 (takeS 100) Goat_unfold.evalO) ()
      , bench "onlineO100"    $ nf (eval0 (takeS 100) Goat_online.evalO) ()
      , bench "offlineO10^3"   $ nf (eval0 (takeS 1000) Goat_unfold.evalO) ()
      , bench "onlineO10^3"    $ nf (eval0 (takeS 1000) Goat_online.evalO) ()
      , bench "offlineO10^4"   $ nf (eval0 (takeS 10000) Goat_unfold.evalO) ()
      , bench "onlineO10^4"    $ nf (eval0 (takeS 10000) Goat_online.evalO) ()
      , bench "offlineO10^5"   $ nf (eval0 (takeS 100000) Goat_unfold.evalO) ()
      , bench "onlineO10^5"    $ nf (eval0 (takeS 100000) Goat_online.evalO) ()
      -- , bench "conspdO"    $ nf (eval25 (takeS 50) Goat_conspd._________evalO) (listGen, listGen, listGen, listGen, listGen,
      --                                                                           listGen, listGen, listGen, listGen, listGen,
      --                                                                           listGen, listGen, listGen, listGen, listGen,
      --                                                                           listGen, listGen, listGen, listGen, listGen,
      --                                                                           listGen, listGen, listGen, listGen, listGen)
     ]
  ]
--
--main =
--  print $ (takeS 10) Goat_online.evalO