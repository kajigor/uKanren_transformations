module Test.PartialDeduction where 

import Test.HUnit (Assertion, (@?=))

import qualified PartialDeduction as PD
import Syntax 
import Program.Programs
import Program.List 
import Text.Printf
import System.Directory
import Printer.Dot
import Printer.PDTree ()
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

-- unit_partialDeductionTest = do 
--   runTest PD.topLevel "da" dA 
--   -- runTest "rev" rev 
--   -- runTest "revAcco" revAcco'

unit_nonConjunctiveTest = do 
  runTest PD.nonConjunctive "da" dA 
  -- runTest "rev" rev 
  -- runTest "revAcco" revAcco'


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

unit_isConflicting = do 
  test2 PD.isConflicting [] [] False
  test2 PD.isConflicting [(1, V 2)] [(2, V 3)] False 
  test2 PD.isConflicting [(1, V 2)] [(1, V 2)] False  
  test2 PD.isConflicting [(1, V 4)] [(1, V 3), (3, V 4)] False 
  test2 PD.isConflicting [(1, V 2)] [(1, V 2)] False

  test2 PD.isConflicting [(1, V 2)] [(1, V 3)] True 
  test2 PD.isConflicting [(1, V 2)] [(1, C "" [])] True 
  test2 PD.isConflicting [(1, C "A" [])] [(1, C "B" [])] True 
  test2 PD.isConflicting [(1, C "A" [C "A" []])] [(1, C "A" [C "B" []])] True 

unit_findConflicting = do 
  test PD.findConflicting 
       [[(1, V 2)], [(1, C "a" [])]] 
       [[[(1, V 2)], [(1, C "a" [])]]]
  test PD.findConflicting 
       [[(1, V 2)], [(1, C "a" [])], [(2, V 3)]] 
       [[[(1, V 2)], [(1, C "a" [])]], [[(2, V 3)]]]
  test PD.findConflicting 
       [[(1, V 2)], [(1, C "a" []), (2, C "b" [])], [(2, V 3)]] 
       [[[(1, V 2)], [(1, C "a" []), (2, C "b" [])], [(2, V 3)]]]
  