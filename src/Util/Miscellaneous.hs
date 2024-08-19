module Util.Miscellaneous where

import           Data.List       (intercalate, subsequences, (\\))
import           Text.Printf     (printf)
import           Util.ListZipper

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

trd4 :: (a, b, c, d) -> c
trd4 (_, _, x, _) = x

lst4 :: (a, b, c, d) -> d
lst4 (_, _, _, x) = x

map1in2 :: (a -> b) -> (a, c) -> (b, c)
map1in2 f (x, y) = (f x, y)

map1in3 :: (a -> b) -> (a, c, d) -> (b, c, d)
map1in3 f (x, y, z) = (f x, y, z)

map2in3 :: (a -> b) -> (c, a, d) -> (c, b, d)
map2in3 f (x, y, z) = (x, f y, z)

map3in3 :: (a -> b) -> (c, d, a) -> (c, d, b)
map3in3 f (x, y, z) = (x, y, f z)

map1in4 :: (a -> b) -> (a, c, d, e) -> (b, c, d, e)
map1in4 f (x, y, z, t) = (f x, y, z, t)

map2in4 :: (a -> b) -> (c, a, d, e) -> (c, b, d, e)
map2in4 f (x, y, z, t) = (x, f y, z, t)

map3in4 :: (a -> b) -> (c, d, a, e) -> (c, d, b, e)
map3in4 f (x, y, z, t) = (x, y, f z, t)

map4in4 :: (a -> b) -> (c, d, e, a) -> (c, d, e, b)
map4in4 f (x, y, z, t) = (x, y, z, f t)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

generateSplits :: Eq a => [a] -> Int -> [([a], [a])]
generateSplits xs n =
  let sub = filter (\x -> n == length x) $ subsequences xs in
  [ (x, xs \\ x) | x <- sub ]

pinpoint :: (a -> Bool) -> [a] -> Maybe (Zipper a)
pinpoint p l = do
  z <- toZipper l
  goRightUntil p z

show' :: Show a => [a] -> String
show' xs =
  intercalate "\n" $ map show xs

escapeTick :: String -> String
escapeTick = concatMap (\x -> if x == '\'' then "\\\'" else [x])

parenthesize x | ' ' `elem` x = printf "(%s)" x
parenthesize x = x

showList :: Show a => String -> [a] -> String
showList padding = unlines . map (printf "%s%s" padding . show)
