module Test.PartialDeduction where 

import Test.HUnit (Assertion, (@?=))

import qualified PartialDeduction as PD
import Syntax 
import CPD
import Miscellaneous
import Programs
import List 
import Text.Printf
import System.Directory
import Printer.Dot
import Printer.PDTree
import System.Process (system)

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output 

test2 :: (Eq c, Show c) => (a -> b -> c) -> a -> b -> c -> Assertion
test2 f input1 input2 output = f input1 input2 @?= output 

manyAssert :: (Eq a, Show a) => a -> (b -> с -> a) -> [(b, с)] -> Assertion
manyAssert expected f =
  mapM_ (\(x, y) -> test2 f x y expected)

dA = doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])

unit_partialDeductionTest = do 
  -- runTest "da" dA 
  -- runTest "rev" rev 
  runTest "revAcco" revAcco'


runTest filename goal = do
  let (tree, logicGoal, names) = PD.topLevel goal
  let path = printf "test/out/pd/%s" filename 
  exists <- doesDirectoryExist path
  if exists 
  then removeDirectoryRecursive path 
  else return () 
  createDirectoryIfMissing True path
  printTree (printf "%s/tree.dot" path) tree 
  system (printf "dot -O -Tpdf %s/*.dot" path)
  return ()
