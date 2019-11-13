module TestFramework where 

import Text.Printf
import Data.Maybe
import Data.List
import Control.Monad


reportError :: Show a => String -> a -> a -> IO ()
reportError name expected actual =
  putStrLn $ printf "%s failed\nExpected: %s\nActual:   %s" name (show expected) (show actual)

assertCustom :: Show a => String -> (a -> a -> Bool) -> a -> a -> IO ()
assertCustom name check expected actual =
  unless (check expected actual) $ reportError name expected actual

anyAssertCustom :: Show a => String -> (a -> a -> Bool) -> [a] -> a -> IO ()
anyAssertCustom name check expected actual =
  mapM_ (\e -> assertCustom name check e actual) expected

manyAssertCustom :: Show a => String -> (a -> a -> Bool) -> [a] -> [a] -> IO ()
manyAssertCustom name check expected actual =
  unless (all (\a -> isJust $ find (check a) expected) actual) $ putStrLn (printf "%s failed\nExpected: %s\nActual:   %s" name (show expected) (show actual))

assert :: (Show a, Eq a) => String -> a -> a -> IO ()
assert name =
  assertCustom name (==)

manyAssert :: (Show a, Eq a) => String -> a -> (b -> с -> a) -> [(b, с)] -> IO ()
manyAssert name expected f =
  mapM_ (\(x, y) -> assert name expected (f x y))

manyAssertOne :: (Show a, Eq a) => String -> a -> (b -> a) -> [b] -> IO ()
manyAssertOne name expected f =
  mapM_ (assert name expected . f)