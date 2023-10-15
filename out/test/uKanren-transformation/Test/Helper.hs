module Test.Helper
  ( Assertion
  , (@?=)
  , test
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , manyAssert
  , assertCustom
  , assertCustom1
  , assertBool
  , manyAssertCustom
  , testTrue
  , test2True
  , test3True
  , testFalse
  , test2False
  , test3False
  ) where

import           Data.List        (find)
import           Data.Maybe       (isJust)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output =
    f input @?= output

testTrue :: (a -> Bool) -> a -> Assertion
testTrue f input = 
  test f input True 

testFalse :: (a -> Bool) -> a -> Assertion
testFalse f input = 
  test f input False

test2 :: (Eq c, Show c) => (a -> b -> c) -> a -> b -> c -> Assertion
test2 f input =
    test (f input)

test2True :: (a -> b -> Bool) -> a -> b -> Assertion
test2True f input = 
  testTrue (f input) 

test2False :: (a -> b -> Bool) -> a -> b -> Assertion
test2False f input = 
  testFalse (f input)

test3 :: (Eq d, Show d) => (a -> b -> c -> d) -> a -> b -> c -> d -> Assertion
test3 f input =
    test2 (f input)

test3True :: (a -> b -> c -> Bool) -> a -> b -> c -> Assertion
test3True f input = 
  test2True (f input) 

test3False :: (a -> b -> c -> Bool) -> a -> b -> c -> Assertion
test3False f input = 
  test2False (f input)

test4 :: (Eq e, Show e) => (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e -> Assertion
test4 f input =
    test3 (f input)

test5 :: (Eq f, Show f) => (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> f -> Assertion
test5 f input =
    test4 (f input)
  
test6 :: (Eq g, Show g) => (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g -> Assertion
test6 f input =
    test5 (f input)

test7 :: (Eq h, Show h) => (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> h -> Assertion
test7 f input =
    test6 (f input)

manyAssert :: (Eq a, Show a) => a -> (b -> с -> a) -> [(b, с)] -> Assertion
manyAssert expected f =
  mapM_ (\(x, y) -> test2 f x y expected)

assertCustom1 :: (Eq a, Show a) => String -> (a -> Bool) -> a -> Assertion
assertCustom1 str fun x =
  assertBool str (fun x)

assertCustom :: (Eq a, Show a) => String -> (a -> b -> Bool) -> a -> b -> Assertion
assertCustom str fun x y =
  assertBool str (fun x y)

manyAssertCustom :: (Eq a, Show a) => String -> (a -> a -> Bool) -> [a] -> [a] -> Assertion
manyAssertCustom str check expected actual =
    assertBool str (all (\a -> isJust $ find (check a) expected) actual)
