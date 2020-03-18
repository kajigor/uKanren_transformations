module Test.Helper
  ( Assertion
  , (@?=)
  , test
  , test2
  , test3
  , test4
  , test5
  , manyAssert
  ) where

import           Test.Tasty.HUnit (Assertion, (@?=))

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output =
    f input @?= output

test2 :: (Eq c, Show c) => (a -> b -> c) -> a -> b -> c -> Assertion
test2 f input =
    test (f input)

test3 :: (Eq d, Show d) => (a -> b -> c -> d) -> a -> b -> c -> d -> Assertion
test3 f input =
    test2 (f input)

test4 :: (Eq e, Show e) => (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e -> Assertion
test4 f input =
    test3 (f input)

test5 :: (Eq f, Show f) => (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> f -> Assertion
test5 f input =
    test4 (f input)

manyAssert :: (Eq a, Show a) => a -> (b -> с -> a) -> [(b, с)] -> Assertion
manyAssert expected f =
  mapM_ (\(x, y) -> test2 f x y expected)
