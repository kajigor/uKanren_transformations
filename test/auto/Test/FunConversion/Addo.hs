{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.FunConversion.Addo where

import Program.Num
import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S
import Data.List (subsequences)
import Control.Monad (msum, guard)
import Stream
import Test.HUnit (assertBool)

$(return $ S.embedProg "addo" $ TR.transMultiMode addo [("addo", x) | x <- subsequences [0..2]])

genNat :: Stream Term
genNat = return O `mplus` (S <$> genNat)

assertAnswerExists :: Eq a => String -> a -> [a] -> IO ()
assertAnswerExists testName candidate answers =
  assertBool testName (candidate `elem` answers)

testAddoIOI :: Int -> Term -> Term -> Term -> IO ()
testAddoIOI n x y z =
  let answers = takeS n $ addoIOI x z in
  assertAnswerExists "addoOIO" y answers

testAddoOIO :: Int -> Term -> Term -> Term -> IO ()
testAddoOIO n x y z =
  let answers = takeS n $ addoOIO y in
  assertAnswerExists "addoIOI" (x,z) answers

testAddoOOI :: Int -> Term -> Term -> Term -> IO ()
testAddoOOI n x y z =
  let answers = takeS n $ addoOOI z in
  assertAnswerExists "addoOOI" (x, y) answers

testAddoIOO :: Int -> Term -> Term -> Term -> IO ()
testAddoIOO n x y z =
  let answers = takeS n $ addoIOO x genNat in
  assertAnswerExists "addoIOO" (y, z) answers

testAddoIIO :: Int -> Term -> Term -> Term -> IO ()
testAddoIIO n x y z =
  let answers = takeS n $ addoIIO x y in
  assertAnswerExists "addoIIO" z answers

testAddoOII :: Int -> Term -> Term -> Term -> IO ()
testAddoOII n x y z =
  let answers = takeS n $ addoOII y z in
  assertAnswerExists "addoOII" x answers

testAddoIII :: Int -> Term -> Term -> Term -> IO ()
testAddoIII n x y z =
  let answers = takeS n $ addoIII x y z in
  assertAnswerExists "addoIII" () answers

runTestFtb :: Int -> IO ()
runTestFtb n =
  let generated :: [(Term, Term, Term)] = takeS n $ (addoOOO genNat :: Stream (Term, Term, Term)) in
  mapM_ (\(x, y, z) -> testAll x y z) generated
  where
    testAll x y z = do
      testAddoIII n x y z
      testAddoIIO n x y z
      testAddoIOI n x y z
      testAddoOII n x y z
      testAddoIOO n x y z
      testAddoOIO n x y z
      testAddoOOI n x y z

unit_TestAddoGen :: IO ()
unit_TestAddoGen =
  runTestFtb 50

