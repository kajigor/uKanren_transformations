module Stream where
import Data

mzero :: Stream a
mzero = Empty

-- used for disjunctions, interleaves streams
mplus :: Stream a -> Stream a -> Stream a
mplus Empty r = r
mplus (Mature h t) r = Mature h (r `mplus` t)
mplus (Immature l) r = Immature (r `mplus` l)

-- used for conjuctions
bind :: Stream  a -> (a -> Stream a) -> Stream a
bind Empty g = mzero
bind (Mature h t) g = g h `mplus` bind t g
bind (Immature l) r = Immature (bind l r)
