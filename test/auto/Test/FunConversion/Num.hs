{-# LANGUAGE TemplateHaskell #-}
module Test.FunConversion.Num where

import Program.Num
import Program (Program(Program))
import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S

import Control.Monad (msum, guard)
import Stream

import Test.HUnit (assertEqual)

$(return $ S.embedProg "mulo" $ TR.transMultiMode mulo [("mulo", [0, 1]), ("mulo", [1, 2])])

runTestFtb :: Term -> Term -> IO ()
runTestFtb a b = let [c] = takeS 1 $ muloIIO a b in 
    let [a'] = takeS 1 $ muloOII b c in assertEqual "num" a a'

unit_TestMulDirections = runTestFtb (S (S O)) (S (S (S O)))

-- TODO: Template test, run with main