module FunConversion.DetStream (takeS, takeWhileS, maybeToStream, isMature, fmap, pure, (<*>), empty, (<|>), (>>=), mzero, mplus, DetStream (..)) where

import           Control.Applicative
import           Control.Monad
import qualified Stream

data DetStream a = Det (Maybe a) | Undet (Stream.Stream a) deriving Show

takeS :: (Num n, Eq n) => n -> DetStream a -> [a]
takeS 0 _            = []
takeS _ (Det Nothing) = []
takeS _ (Det (Just a)) = [a]
takeS n (Undet s) = Stream.takeS n s

takeWhileS :: (a -> Bool) -> DetStream a -> [a]
takeWhileS p (Det Nothing) = []
takeWhileS p (Det (Just a)) | p a = [a]
                          | otherwise = []
takeWhileS p (Undet s) = Stream.takeWhileS p s


maybeToStream :: Maybe a -> DetStream a
maybeToStream Nothing  = empty
maybeToStream (Just a) = return a

maybeToDetStream :: Maybe a -> DetStream a
maybeToDetStream = Det

isMature :: DetStream a -> Bool
isMature (Det _) = True
isMature (Undet s) = Stream.isMature s

instance Functor DetStream where
    fmap = liftM

instance Applicative DetStream where
    pure a = Undet (pure a)
    (<*>) = ap

instance Alternative DetStream where
    empty = Undet empty
    Det Nothing   <|> y           = y
    x             <|> Det Nothing = x
    x@(Det _)     <|> _           = x
    _             <|> y@(Det _)   = y
    (Undet x)     <|> (Undet y)   = Undet (x <|> y)

immature :: DetStream a -> DetStream a
immature (Det x) = Det x
immature (Undet s) = Undet (Stream.Immature s)

instance Monad DetStream where
    (Det Nothing) >>= _ = Det Nothing
    (Det (Just x)) >>= g = g x
    (Undet Stream.Empty) >>= g = mzero
    (Undet (Stream.Mature x s)) >>= g = g x `mplus` (Undet s >>= g)
    (Undet (Stream.Immature s)) >>= g = immature $ Undet s >>= g

instance MonadPlus DetStream where
