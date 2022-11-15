module Test.Mode.Analysis where

import Program.Num
import Syntax
import Program
import Mode.Toplevel
import VarRename
import Test.Helper ((@?=))

unit_analysis :: IO ()
unit_analysis = do
  print $ topLevel (Program addo (fresh ["x", "y", "z"] $ call "addo" [V "x", V "y", V "z"])) [0, 1]