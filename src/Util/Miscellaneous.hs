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
  let sub = subsequencesOfLength n xs in
  [ (x, xs \\ x) | x <- sub ]

-- | @subsequencesOfLength k xs == filter ((== k) . length) (subsequences xs)@,
--   in the same order, but without enumerating all 2^n subsequences: the
--   traversal of 'subsequences' is followed and pruned to the lengths that
--   can still become @k@.  (Filtering made residualization exponential in the
--   length of a conjunction even when the full-length candidate, which is
--   tried first, succeeds.)
subsequencesOfLength :: Int -> [a] -> [[a]]
subsequencesOfLength k xs
  | k == 0    = [[]]
  | k < 0     = []
  | otherwise = map snd (nes k k xs)
  where
    -- non-empty subsequences in the order of Data.List.subsequences, restricted
    -- to lengths in [lo, hi], with their lengths
    nes :: Int -> Int -> [a] -> [(Int, [a])]
    nes _ _ [] = []
    nes lo hi (y : ys)
      | hi < 1 = []
      | otherwise =
          [ (1, [y]) | lo <= 1 ] ++
          concat [ [ (l, zs) | lo <= l ] ++ [ (l + 1, y : zs) | lo <= l + 1, l + 1 <= hi ]
                 | (l, zs) <- nes (lo - 1) hi ys, l <= hi ]

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
