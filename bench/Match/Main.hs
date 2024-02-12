{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified Match_online
import qualified Match_offline

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: MonadPlus m => (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

main = defaultMain
  [
    bgroup "match"
     [
        bench "offlineO10"    $ nf (eval0 (takeS 10) Match_offline.matchoO) ()
      , bench "onlineO10"     $ nf (eval0 (takeS 10) Match_online.matchoO) ()
      , bench "offlineO28"    $ nf (eval0 (takeS 28) Match_offline.matchoO) ()
      , bench "onlineO28"     $ nf (eval0 (takeS 28) Match_online.matchoO) ()
      , bench "offlineO36"    $ nf (eval0 (takeS 36) Match_offline.matchoO) ()
      , bench "onlineO36"     $ nf (eval0 (takeS 36) Match_online.matchoO) ()
      , bench "offlineO45"    $ nf (eval0 (takeS 45) Match_offline.matchoO) ()
      , bench "onlineO45"     $ nf (eval0 (takeS 45) Match_online.matchoO) ()

     ]
--    ,bgroup "SortGen"
--    [
--        bench "offlineGen"  $ nf (eval26 (takeS 1) double_appendoIIIOnline) $ natGen23 a b c
--      , bench "onlineGen"   $ nf (eval7 (takeS 1) sortoOOnline) genSort -- failing
--    ]
  ]


--main =
--  print $ (takeS 5) $ DoubleAppend_cpd_ans.double_appendoOOO listGen listGen listGen listGen listGen listGen

