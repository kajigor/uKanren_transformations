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
import qualified Match_simple

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: MonadPlus m => (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

pt = Cons (S (S O)) (Cons (S O) (Cons (S O) (Cons O (Cons O (Cons (S (S O)) (Cons O (Cons (S O) Nil)))))))

main = defaultMain
  [
    bgroup "match"
     [
        bench "offlineO45"    $ nf (eval0 (takeS 45) Match_offline.matchodsO) ()
      , bench "onlineO45"     $ nf (eval0 (takeS 45) Match_online.matchoO) ()
      , bench "simple045"     $ nf (eval (takeS 45) Match_simple.matcho) pt
     ]
  ]
