{-# LANGUAGE DeriveFunctor #-}

module Util.ListZipper where

import Data.List (partition)

newtype Zipper a = Zipper { getZipper :: ([a], a, [a]) }
                 deriving (Show, Eq, Functor)

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

allRights :: Zipper a -> [Zipper a]
allRights zipper = 
  zipper : maybe [] allRights (goRight zipper)  

replaceCursor :: a -> Zipper a -> Zipper a
replaceCursor x (Zipper (ls, _, rs)) = Zipper (ls, x, rs)

goRightUntil :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goRightUntil p z | p (cursor z) = Just z
goRightUntil _ z | isRightmost z = Nothing
goRightUntil p z = goRight z >>= goRightUntil p

goRightWhile :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goRightWhile p = goRightUntil (not . p)


prioritizeRightsByPred :: [a -> Bool] -> Zipper a -> [Zipper a]
prioritizeRightsByPred ps xs =
  let zippers = allRights xs
   in go ps zippers
  where
    go [] zippers = zippers
    go (p : ps) zippers =
      let (yes, no) = partition (p . cursor) zippers
       in yes ++ go ps no

prioritizeByPred :: [a -> Bool] -> [a] -> [Zipper a]
prioritizeByPred ps xs = 
    let zippers = allZippers xs in 
    go ps zippers 
  where 
    go [] zippers = zippers 
    go (p:ps) zippers = 
      let (yes, no) = partition (p . cursor) zippers in 
      yes ++ go ps no 
    
allZippers :: [a] -> [Zipper a] 
allZippers = 
    maybe [] go . toZipper 
  where 
    go :: Zipper a -> [Zipper a]
    go z = z : maybe [] go (goRight z)
