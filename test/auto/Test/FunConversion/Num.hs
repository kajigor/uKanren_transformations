module Test.FunConversion.Num where

import qualified Test.FunConversion.NumForward as NF
import qualified Test.FunConversion.NumBackward as NB
import Stream
import Test.HUnit (assertEqual)

ftb :: NF.Term -> NB.Term
ftb NF.O = NB.O
ftb (NF.S x) = NB.S (ftb x)

btf :: NB.Term -> NF.Term
btf NB.O = NF.O
btf (NB.S x) = NF.S (btf x)

runTestFtb :: NF.Term -> NF.Term -> IO ()
runTestFtb a b = let [c] = takeS 1 $ NF.muloIIO a b in 
    let [a'] = takeS 1 $ NB.muloOII (ftb b) (ftb c) in assertEqual "num" a (btf a')

unit_TestMulDirections = runTestFtb (NF.S (NF.S NF.O)) (NF.S (NF.S (NF.S NF.O)))