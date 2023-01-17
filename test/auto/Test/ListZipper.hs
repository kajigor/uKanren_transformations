module Test.ListZipper where

import           Test.Helper     (test2)

import           Util.ListZipper

xs :: Zipper Integer
xs = Zipper ([], 1, [2,3,4])

unit_goRightUntil = do
    test2 goRightUntil (== 0) xs Nothing
    test2 goRightUntil (== 1) xs (Just xs)
    test2 goRightUntil (== 2) xs (Just $ Zipper ([1], 2, [3,4]))
    test2 goRightUntil (== 3) xs (Just $ Zipper ([2,1], 3, [4]))
    test2 goRightUntil (== 4) xs (Just $ Zipper ([3,2,1], 4, []))
    test2 goRightUntil (== 5) xs Nothing

unit_goRightWhile = do
    test2 goRightWhile (== 0) xs (Just xs)
    test2 goRightWhile (== 1) xs (Just $ Zipper ([1], 2, [3,4]))
    test2 goRightWhile (<= 2) xs (Just $ Zipper ([2,1], 3, [4]))
    test2 goRightWhile (<= 3) xs (Just $ Zipper ([3,2,1], 4, []))
    test2 goRightWhile (<= 5) xs Nothing
