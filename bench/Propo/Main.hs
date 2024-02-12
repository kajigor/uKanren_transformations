{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)
import Term

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified Prop_offline
import qualified Prop_online

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

main = defaultMain
  [
    bgroup "RelSort"
     [
        bench "offlineO10"   $ nf (eval0 (takeS 10) Prop_offline.evaloO) ()
      , bench "onlineO10"    $ nf (eval0 (takeS 10) Prop_online.evaloO) ()
      , bench "offlineO50"   $ nf (eval0 (takeS 50) Prop_offline.evaloO) ()
      , bench "onlineO50"    $ nf (eval0 (takeS 50) Prop_online.evaloO) ()
      , bench "offlineO200"   $ nf (eval0 (takeS 200) Prop_offline.evaloO) ()
      , bench "onlineO200"    $ nf (eval0 (takeS 200) Prop_online.evaloO) ()
     ]
  ]


--main = mapM_ (print. (takeS 10)) [Prop_offline.evaloO, Prop_online.evaloO]
--  print $ (takeS 10) Prop_offline.evaloO
--  print $ (takeS 10) Prop_online.evaloO