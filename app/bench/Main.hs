module Main where

import           Program.Programs
import           Syntax
import qualified Transformer.ConsPD

runConsPD = Transformer.ConsPD.runConsPD

runDoubleAppendo =
    runConsPD (-1) "doubleAppendo" prog
  where
    prog = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])

main :: IO ()
main =
  runDoubleAppendo
