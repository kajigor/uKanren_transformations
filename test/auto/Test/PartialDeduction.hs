module Test.PartialDeduction where

import           Test.Helper (test, test2, manyAssert)

import qualified PartialDeduction as PD
import           Printer.Dot
import           Printer.PDTree   ()
import           Program.List     (nil, revAcco, reverso, (%))
import           Program.Programs (doubleAppendo)
import qualified Program.Prop
import           Syntax
import           System.Directory
import           System.Process   (system)
import           Text.Printf

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
prop = Program.Prop.query3

unit_partialDeductionTest = do
  runTest PD.topLevel "da" dA
  runTest PD.topLevel "rev" rev
  runTest PD.topLevel "revAcco" revAcco'
  runTest PD.topLevel "prop" prop

runTest function filename goal = do
  let (tree, logicGoal, names) = function goal
  let path = printf "test/out/pd/%s" filename
  exists <- doesDirectoryExist path
  if exists
  then removeDirectoryRecursive path
  else return ()
  createDirectoryIfMissing True path
  printTree (printf "%s/tree.dot" path) tree
  system (printf "dot -O -Tpdf %s/*.dot" path)
  return ()
