{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus, mfilter)
import Term

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import qualified Prop_offline
import qualified Prop_online
import qualified Prop_simple

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x,y,z) -> f x y z

-- lst = eval0 (takeS 10) Prop_offline._evalosdsO ()

-- natGen = return Zero <|> (Succ <$> natGen)

a0 = (False, False)
a1 = (True, False)
a2 = (False, True) 
a3 = (True, True)
genP :: MonadPlus m => m (Bool, Bool)
genP = (return a0) <|> (return a1) <|> (return a2) <|> (return a3)

evalP :: MonadPlus m => Bool -> m Term
evalP val = do 
  (x, y) <- mfilter 
    (((==) val) . 
      (uncurry (&&))) genP
  u <- evalP x 
  v <- evalP y 
  return (Conj u v) 

main = defaultMain
  [
    bgroup "Propo"
     [
        bench "offlineO10"   $ nf (eval0 (takeS 10) Prop_offline._evalosdsO) ()
      , bench "onlineO10"    $ nf (eval0 (takeS 10) Prop_online.evaloO) ()
      , bench "simple10"     $ nf (eval2 (takeS 10) Prop_simple.evaloIOI) (Cons (Trueo) (Cons (Trueo) Nil), Trueo)
      , bench "offlineO10^3"   $ nf (eval0 (takeS 1000) Prop_offline._evalosdsO) ()
      , bench "onlineO10^3"    $ nf (eval0 (takeS 1000) Prop_online.evaloO) ()
      , bench "simple10^3"     $ nf (eval2 (takeS 1000) Prop_simple.evaloIOI) (Cons (Trueo) (Cons (Trueo) Nil), Trueo)
      , bench "offlineO10^5"   $ nf (eval0 (takeS 100000) Prop_offline._evalosdsO) ()
      , bench "onlineO10^5"    $ nf (eval0 (takeS 100000) Prop_online.evaloO) ()
      , bench "simple10^5"     $ nf (eval2 (takeS 100000) Prop_simple.evaloIOI) (Cons (Trueo) (Cons (Trueo) Nil), Trueo)
      , bench "offlineO10^6"   $ nf (eval0 (takeS 1000000) Prop_offline._evalosdsO) ()
      , bench "onlineO10^6"    $ nf (eval0 (takeS 1000000) Prop_online.evaloO) ()
      , bench "simple10^6"     $ nf (eval2 (takeS 1000000) Prop_simple.evaloIOI) (Cons (Trueo) (Cons (Trueo) Nil), Trueo)
     ]
  ]


--main = mapM_ (print. (takeS 10)) [Prop_offline.evaloO, Prop_online.evaloO]
--  print $ (takeS 10) Prop_offline.evaloO
--  print $ (takeS 10) Prop_online.evaloO