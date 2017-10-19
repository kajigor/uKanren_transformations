module Stream where

import Control.Monad
import Control.Applicative

-- Stream
data Stream a = Empty
              | Mature a (Stream a)
              -- we need this in case of left recursion (who would have known)
              | Immature (Stream a)
              deriving Show

takeS 0 _            = []
takeS n Empty        = []
takeS n (Mature a s) = a : takeS (n-1) s
takeS n (Immature s) = takeS n s

maybeToStream Nothing  = Empty
maybeToStream (Just a) = return a

instance Functor Stream where
  fmap _ Empty        = Empty
  fmap f (Mature a s) = Mature (f a) (fmap f s)
  fmap f (Immature s) = Immature (fmap f s)

instance Applicative Stream where
  pure a = Mature a Empty
  Empty        <*> _            = Empty
  (Mature f s) <*> Empty        = Empty
  (Immature s) <*> x            = s <*> x
  (Mature f s) <*> (Mature x t) = Mature (f x) (s <*> t)
  s            <*> (Immature t) = s <*> t

instance Alternative Stream where
  empty = Empty
  Empty <|> s = s
  s <|> Empty = s
  Immature s <|> t = s <|> t
  s <|> Immature t = s <|> t
  Mature a s <|> t = Mature a (s <|> t)

instance Monad Stream where
  Empty >>= _ = mzero
  Mature x xs >>= g = g x `mplus` (xs >>= g)
  Immature x  >>= y = Immature $ x >>= y

instance MonadPlus Stream where
  mzero = Empty
  mplus (Mature h tl) y = Mature h $ y `mplus` tl
  mplus (Immature  x) y = Immature $ y `mplus` x
  mplus Empty         y = y
