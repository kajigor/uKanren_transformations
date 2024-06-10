module FunConversion.SemiStream (takeS, takeWhileS, maybeToStream, isMature, fmap, pure, (<*>), empty, (<|>), (>>=), mzero, mplus, SemiStream (..)) where

import           Control.Applicative
import           Control.Monad
import qualified Stream as FullStream

-- Stream
data SemiStream a = Empty
              | Det a
              | Mature a (SemiStream a)
              -- we need this in case of left recursion (who would have known)
              | Immature (SemiStream a)
              deriving Show

toFullStream :: SemiStream a -> FullStream.Stream a
toFullStream Empty = FullStream.Empty
toFullStream (Det a) = return a
toFullStream (Mature a t) = FullStream.Mature a (toFullStream t)
toFullStream (Immature x) = FullStream.Immature (toFullStream x)

toSemiStream :: FullStream.Stream a -> SemiStream a
toSemiStream FullStream.Empty = Empty
toSemiStream (FullStream.Mature a t) = Mature a (toSemiStream t)
toSemiStream (FullStream.Immature x) = Immature (toSemiStream x)

takeS :: (Num n, Eq n) => n -> SemiStream a -> [a]
takeS 0 _            = []
takeS _ Empty        = []
takeS _ (Det a)      = [a]
takeS n (Mature a s) = a : takeS (n-1) s
takeS n (Immature s) = takeS n s

takeWhileS :: (a -> Bool) -> SemiStream a -> [a]
takeWhileS p Empty = []
takeWhileS p (Det a) | p a = [a]
                     | otherwise = []
takeWhileS p (Mature a s) | p a = a : takeWhileS p s
                          | otherwise = takeWhileS p s
takeWhileS p (Immature s) = takeWhileS p s


maybeToStream :: Maybe a -> SemiStream a
maybeToStream Nothing  = Empty
maybeToStream (Just a) = return a

maybeToDetStream :: Maybe a -> SemiStream a
maybeToDetStream Nothing = Empty
maybeToDetStream (Just a) = Det a

isMature :: SemiStream a -> Bool
isMature (Immature _) = False
isMature _ = True

instance Functor SemiStream where
    fmap = liftM

instance Applicative SemiStream where
    pure a = Mature a Empty
    (<*>) = ap

instance Alternative SemiStream where
    empty = Empty
    Empty         <|> y = y
    x@(Det _)     <|> _ = x
    -- _             <|> y@(Det _) = y
    (Mature h tl) <|> y = Mature h $ y <|> tl
    (Immature  x) <|> y = Immature $ y <|> x

instance Monad SemiStream where
    Empty >>= _ = mzero
    (Det x) >>= g = g x
    Mature x xs >>= g = g x `mplus` (xs >>= g)
    Immature x  >>= y = Immature $ x >>= y

instance MonadPlus SemiStream where
