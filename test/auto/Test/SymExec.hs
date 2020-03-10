module Test.SymExec where

import           Test.Helper (test, test2, manyAssert)

import           SymbolicExecution (topLevel)
import           Printer.Dot
import           Printer.SymTree
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

unit_nonConjunctiveTest = do
  runTest (topLevel 5) "da" dA
  runTest (topLevel 5) "rev" rev
  runTest (topLevel 5) "revAcco" revAcco'

runTest function filename goal = do
  let tree = function goal
  let path = printf "test/out/sym/%s" filename
  exists <- doesDirectoryExist path
  if exists
  then removeDirectoryRecursive path
  else return ()
  createDirectoryIfMissing True path
  printTree (printf "%s/tree.dot" path) tree
  system (printf "dot -O -Tpdf %s/*.dot" path)
  return ()