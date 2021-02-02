module Util.ListZipper where

newtype Zipper a = Zipper { getZipper :: ([a], a, [a]) }
                 deriving (Show, Eq)

left :: Zipper a -> [a]
left (Zipper (ls, _, _)) = reverse ls

right :: Zipper a -> [a]
right (Zipper (_, _, rs)) = rs

cursor :: Zipper a -> a
cursor (Zipper (_, x, _)) = x

isRightmost :: Zipper a -> Bool
isRightmost = null . right

isLeftmost :: Zipper a -> Bool
isLeftmost = null . left

toZipper :: [a] -> Maybe (Zipper a)
toZipper [] = Nothing
toZipper (h:t) = Just $ Zipper ([], h, t)

fromZipper :: Zipper a -> [a]
fromZipper z = left z ++ cursor z : right z

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft z | isLeftmost z = Nothing
goLeft (Zipper (h:ls, x, rs)) = Just $ Zipper (ls, h, x:rs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight z | isRightmost z = Nothing
goRight (Zipper (ls, x, h:rs)) = Just $ Zipper (x:ls, h, rs)

replaceCursor :: a -> Zipper a -> Zipper a
replaceCursor x (Zipper (ls, _, rs)) = Zipper (ls, x, rs)

goRightUntil :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goRightUntil p z | p (cursor z) = Just z
goRightUntil _ z | isRightmost z = Nothing
goRightUntil p z = goRight z >>= goRightUntil p

goRightWhile :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goRightWhile p = goRightUntil (not . p)