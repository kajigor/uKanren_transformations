{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.FunConversion.Mulo where

import Program.Num
import Program (Program(Program))
import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S
import Data.List (subsequences)
import Control.Monad (msum, guard)
import Stream
import Test.HUnit (assertBool)

$(return $ S.embedProg "mulo" $ TR.transMultiMode mulo [("mulo", x) | x <- subsequences [0..2]])

genNat :: Stream Term
genNat = return O `mplus` (S <$> genNat)

assertAnswerExists :: Eq a => String -> a -> [a] -> IO ()
assertAnswerExists testName candidate answers =
  assertBool testName (candidate `elem` answers)

testMuloIOI :: Int -> Term -> Term -> Term -> IO ()
testMuloIOI n x y z =
  let answers = takeS n $ muloIOI x z genNat genNat in
  assertAnswerExists "muloOIO" y answers

testMuloOIO :: Int -> Term -> Term -> Term -> IO ()
testMuloOIO n x y z =
  let answers = takeS n $ muloOIO y in
  assertAnswerExists "muloIOI" (x,z) answers

testMuloOOI :: Int -> Term -> Term -> Term -> IO ()
testMuloOOI n x y z =
  let answers = takeS n $ muloOOI z genNat genNat in
  assertAnswerExists "muloOOI" (x, y) answers

testMuloIOO :: Int -> Term -> Term -> Term -> IO ()
testMuloIOO n x y z =
  let answers = takeS n $ muloIOO x genNat in
  assertAnswerExists "muloIOO" (y, z) answers

testMuloIIO :: Int -> Term -> Term -> Term -> IO ()
testMuloIIO n x y z =
  let answers = takeS n $ muloIIO x y in
  assertAnswerExists "muloIIO" z answers

testMuloOII :: Int -> Term -> Term -> Term -> IO ()
testMuloOII n x y z =
  let answers = takeS n $ muloOII y z in
  assertAnswerExists "muloOII" x answers

testMuloIII :: Int -> Term -> Term -> Term -> IO ()
testMuloIII n x y z =
  let answers = takeS n $ muloIII x y z in
  assertAnswerExists "muloIII" () answers

le :: Term -> Int -> Bool
le O _ = True
le (S x) n | n > 0 = le x (n - 1)
           | otherwise = False

runTestFtb :: Int -> IO ()
runTestFtb m = do
  let n = length $ [ True | x <- [0..m], y <- [0..m], z <- [0..m], x * y == z]
  let generated :: [(Term, Term, Term)] = take n $ takeWhileS (\(x, y, z) -> le x m && le y m && le z m) (muloOOO genNat :: Stream (Term, Term, Term))
  mapM_ (\(x, y, z) -> testAll n x y z) generated
  where
    testAll n x y z = do
      testMuloIII n x y z
      testMuloIIO n x y z
      testMuloIOI n x y z
      testMuloOII n x y z
      testMuloIOO n x y z
      testMuloOIO n x y z
      testMuloOOI n x y z

unit_TestMuloGen :: IO ()
unit_TestMuloGen =
  runTestFtb 10

