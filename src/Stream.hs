{-# LANGUAGE InstanceSigs #-}
module Stream (takeS, takeWhileS, maybeToStream, isMature, fmap, pure, (<*>), empty, (<|>), (>>=), mzero, mplus, Stream (..)) where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Fail as Fail

-- Stream
data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion (who would have known)
              | Immature (Stream a)
              deriving Show

takeS :: (Num n, Eq n) => n -> Stream a -> [a]
takeS 0 _            = []
takeS _ Empty        = []
takeS n (Mature a s) = a : takeS (n-1) s
takeS n (Immature s) = takeS n s

takeWhileS :: (a -> Bool) -> Stream a -> [a]
takeWhileS p Empty = []
takeWhileS p (Mature a s) | p a = a : takeWhileS p s
                          | otherwise = takeWhileS p s
takeWhileS p (Immature s) = takeWhileS p s


maybeToStream :: Maybe a -> Stream a
maybeToStream Nothing  = Empty
maybeToStream (Just a) = return a

isMature :: Stream a -> Bool
isMature (Immature _) = False
isMature _ = True

instance Functor Stream where
  fmap _ Empty        = Empty
  fmap f (Mature a s) = Mature (f a) (fmap f s)
  fmap f (Immature s) = Immature (fmap f s)

instance Applicative Stream where
  pure a = Mature a Empty
  Empty        <*> _            = Empty
  (Mature _ _) <*> Empty        = Empty
  (Immature s) <*> x            = s <*> x
  (Mature f s) <*> (Mature x t) = Mature (f x) (s <*> t)
  s            <*> (Immature t) = s <*> t

instance Alternative Stream where
  empty = Empty
  (Mature h tl) <|> y = Mature h $ y <|> tl
  (Immature  x) <|> y = Immature $ y <|> x
  Empty         <|> y = y

instance Monad Stream where
  Empty >>= _ = mzero
  Mature x xs >>= g = g x <|> (xs >>= g)
  Immature x  >>= y = Immature $ x >>= y

instance MonadPlus Stream where

instance Fail.MonadFail Stream where
  fail _ = empty