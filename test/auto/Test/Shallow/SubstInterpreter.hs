{-# LANGUAGE Rank2Types #-}
module Test.Shallow.SubstInterpreter where

import Shallow.Core.Logic
import Shallow.Core.Kanren
import Shallow.Interpreters.SubstKanren
import Test.Shallow.Relations.Addo
import Test.Shallow.Types.Nat
import Stream

import Test.HUnit (assertBool)
import Data.Maybe (fromJust)


assertAnswerExists :: Eq a => String -> Int -> a -> Stream a -> IO ()
assertAnswerExists testName n candidate answers =
  assertBool testName (candidate `elem` (takeS n $ answers))

testAddoIIO :: Natural -> Natural -> IO ()
testAddoIIO x y = assertAnswerExists ("addoIIO " ++ show x ++ " " ++ show y) 1 (x+y) $ runSubstKanren $ run $ \z -> addo (project' x) (project' y) z

unit_addo :: IO ()
unit_addo = do
    testAddoIIO 0 0
    testAddoIIO 1 0
    testAddoIIO 2 0
    testAddoIIO 0 1
    testAddoIIO 1 1
    testAddoIIO 2 1
    testAddoIIO 0 2
    testAddoIIO 1 2
    testAddoIIO 2 2