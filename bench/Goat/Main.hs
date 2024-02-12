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

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: Show r => (m r -> [r]) -> (x1 -> x2 -> m r) -> (x1, x2) -> [r]
eval2 listify f = eval listify  $ \(y1, y2) -> f y1 y2

start :: Term
start = Pair (Quad Term.True Term.True Term.True Term.True) (Quad Term.False Term.False Term.False Term.False)

end :: Term
end = Pair (Quad Term.False Term.False Term.False Term.False) (Quad Term.True Term.True Term.True Term.True)

eval25 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> x22 -> x23 -> x24 -> m r) -> (a, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24) -> [r]
eval25 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24) -> f b y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24

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

elemGen :: (MonadPlus m) => m Term
elemGen = return Goat <|> return Wolf <|> return Cabbage <|> return Term.Empty

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- elemGen
  return Nil <|> (Cons x <$> listGen)

main = defaultMain
  [
    bgroup "Goat"
     [
        bench "offlineO"   $ nf (eval0 (takeS 50) Goat_unfold.evalO) ()
      , bench "onlineO"    $ nf (eval0 (takeS 50) Goat_online.evalO) ()
      , bench "conspdO"    $ nf (eval25 (takeS 50) Goat_conspd._________evalO) (listGen, listGen, listGen, listGen, listGen,
                                                                                listGen, listGen, listGen, listGen, listGen,
                                                                                listGen, listGen, listGen, listGen, listGen,
                                                                                listGen, listGen, listGen, listGen, listGen,
                                                                                listGen, listGen, listGen, listGen, listGen)
     ]
  ]
--
--main =
--  print $ (takeS 10) Goat_online.evalO