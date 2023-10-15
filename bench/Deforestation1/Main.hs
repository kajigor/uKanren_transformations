{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq, Generic, DS.NFData)
    
rrOoff x0 = msum [do {(x1, x3) <- case x0 of
                               {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                   x2 <- case x1 of
                         {S y2 -> return y2; _ -> mzero};
                   guard (x2 == O);
                   guard (x3 == Nil);
                   return ()}]
                   

rrIonline x0 = msum [do {rRI x0; return ()}]
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

main :: IO ()
main = defaultMain
  [
    bgroup "Deforestation1"
     [ bench "offline1"    $ nf (eval (takeS 1) rrOoff) (Cons (S O) Nil) 
     , bench "online1"     $ nf (eval (takeS 1) rrIonline) (Cons (S O) Nil)
     , bench "offline2"    $ nf (eval (takeS 1) rrOoff) (Cons (S O) (Cons O (Cons (S O) Nil)))
     , bench "online2"     $ nf (eval (takeS 1) rrIonline) (Cons (S O) (Cons O (Cons (S O) Nil)))
     ]
  ]