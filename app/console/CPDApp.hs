module CPDApp where

import           Program.List           (maxLengtho, maxMino)
import qualified Program.PropEval
import           Syntax
import qualified Transformer.CPD
import CPD.LocalControl

maxLen = Program maxLengtho $ fresh ["xs", "m", "l"] (call "maxLengtho" [V "xs", V "m", V "l"])
maxMin = Program maxMino $ fresh ["xs", "m", "l"] (call "maxMino" [V "xs", V "m", V "l"])


transform = Transformer.CPD.transform


runMaxlen = do
    transform "maxLengtho" maxLen Nothing Deterministic

runMaxMin = do
    transform "maxMino" maxMin Nothing Deterministic


runPropEval = do
    transform "propFirstNando" Program.PropEval.nandoFirstQuery Nothing Deterministic
    transform "propFirstPlain" Program.PropEval.plainFirstQuery Nothing Deterministic
    transform "propLastPlain"  Program.PropEval.plainLastQuery Nothing Deterministic
    transform "propLastNando"  Program.PropEval.nandoLastQuery Nothing Deterministic


run :: IO ()
run = do
  runMaxlen
  runMaxMin
  runPropEval
