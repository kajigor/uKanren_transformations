module Test.PartialDeduction where

import           Test.HUnit       (Assertion, (@?=))

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

test :: (Eq b, Show b) => (a -> b) -> a -> b -> Assertion
test f input output = f input @?= output

test2 :: (Eq c, Show c) => (a -> b -> c) -> a -> b -> c -> Assertion
test2 f input1 input2 output = f input1 input2 @?= output

test5 :: (Eq f, Show f) => (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> f -> Assertion
test5 f input1 input2 input3 input4 input5 output =
    f input1 input2 input3 input4 input5 @?= output

manyAssert :: (Eq a, Show a) => a -> (b -> с -> a) -> [(b, с)] -> Assertion
manyAssert expected f =
  mapM_ (\(x, y) -> test2 f x y expected)

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
prop = Program.Prop.query3

-- unit_partialDeductionTest = do
--   runTest PD.topLevel "da" dA
--   runTest PD.topLevel "rev" rev
--   runTest PD.topLevel "revAcco" revAcco'
--   runTest PD.topLevel "prop" prop

unit_nonConjunctiveTest = do
  runTest PD.nonConjunctive "da" dA
  -- runTest "rev" rev
  -- runTest "revAcco" revAcco'

unit_unifySubsts = do
  test PD.unifySubsts [] (Just [])
  test PD.unifySubsts [ [(1, V 4), (0, nil)]
                      , [(2, V 3), (4, nil)]
                      ]
                      (Just [(4, nil), (2, V 3), (1, V 4), (0, nil)])
  test PD.unifySubsts [ [(4, V 5 % V 7),  (0, V 5 % V 6)]
                      , [(3, V 8 % V 10), (4, V 8 % V 9)]
                      ]
                      (Just [(7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)])

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
  test2 PD.isConflicting [(1, V 2)] [(1, V 3)] False
  test2 PD.isConflicting [(1, V 2)] [(1, C "" [])] False
  test2 PD.isConflicting [(1, V 9),  (6, nil), (7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)]
                         [(2, V 10), (9, nil), (7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)]
                         False
  test2 PD.isConflicting [(1, V 4), (0, nil)] [(2, V 3), (4, nil)] False
  test2 PD.isConflicting [(1, V 4), (0, nil)] [(3, V 8 % V 10), (4, V 8 % V 9)] False
  test2 PD.isConflicting [(4, V 5 % V 7), (0, V 5 % V 6)] [(3, V 8 % V 10), (4, V 8 % V 9)] False

  test2 PD.isConflicting [(4, V 5 % V 7), (0, V 5 % V 6)] [(2, V 3), (4, nil)] True
  test2 PD.isConflicting [(1, C "A" [])] [(1, C "B" [])] True
  test2 PD.isConflicting [(1, C "A" [C "A" []])] [(1, C "A" [C "B" []])] True
  test2 PD.isConflicting [(1, C "A" [])] [(1, C "A" [C "B" []])] True

unit_findConflicting = do
    test PD.findConflicting
         [[(1, V 2)], [(1, a)]]
         [[[(1, V 2)]], [[(1, a)]]]
    test PD.findConflicting
         [[(1, V 2)], [(1, a)], [(2, V 3)]]
         [[[(1, V 2)]], [[(1, a)]], [[(2, V 3)]]]
    test PD.findConflicting
         [[(1, V 2)], [(1, a), (2, b)], [(2, V 3)]]
         [[[(1, V 2)]], [[(1, a), (2, b)]], [[(2, V 3)]]]
    test PD.findConflicting
         [[(1, b)], [(1, a), (2, b)], [(2, V 3)]]
         [[[(1, b)], [(1, a), (2, b)]], [[(2, V 3)]]]
    test PD.findConflicting
         [[(1, b)], [(1, a), (2, b)], [(2, b)]]
         [[[(1, b)], [(1, a), (2, b)]], [[(2, b)]]]
    test PD.findConflicting
         [[(1, b)], [(1, a), (2, b)], [(2, a)]]
         [[[(1, b)], [(1, a), (2, b)], [(2, a)]]]
    test PD.findConflicting
         [[(1, b)], [(1, a), (2, c b)], [(2, c a)]]
         [[[(1, b)], [(1, a), (2, c b)], [(2, c a)]]]
    test PD.findConflicting
         [[(1, V 9),(6, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)], [(2, V 10),(9, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]]
         [[[(1, V 9),(6, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]], [[(2, V 10),(9, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]]]

  where
    a = C "a" []
    b = C "b" []
    c x = C "c" [x]

unit_productList = do
    test PD.productList [[]] ([] :: [[Int]])
    test PD.productList ([] :: [[Int]]) [[]]
    test PD.productList [[1,2,3]] [[1], [2], [3]]
    test PD.productList [[1,2,3], [4], [5,6]] [[1,4,5], [1,4,6], [2,4,5], [2,4,6], [3,4,5], [3,4,6]]
    test PD.productList [ [ ([], [(1, V 4), (0, nil)])
                          , ([appendo (V 6) (V 1) (V 7)], [(4, (V 5 % V 7)), (0, (V 5 % V 6))])
                          ]
                        , [ ([], [(2, V 3), (4, nil)])
                          , ([appendo (V 6) (V 2) (V 7)], [(3, (V 5 % V 7)), (4, (V 5 % V 6))])
                          ]
                        ]
                        [ [ ([], [(1, V 4), (0, nil)])
                          , ([], [(2, V 3), (4, nil)])
                          ]
                        , [ ([], [(1, V 4), (0, nil)])
                          , ([appendo (V 6) (V 2) (V 7)], [(3, (V 5 % V 7)), (4, (V 5 % V 6))])
                          ]
                        , [ ([appendo (V 6) (V 1) (V 7)], [(4, (V 5 % V 7)), (0, (V 5 % V 6))])
                          , ([], [(2, V 3), (4, nil)])
                          ]
                        , [ ([appendo (V 6) (V 1) (V 7)], [(4, (V 5 % V 7)), (0, (V 5 % V 6))])
                          , ([appendo (V 6) (V 2) (V 7)], [(3, (V 5 % V 7)), (4, (V 5 % V 6))])
                          ]
                        ]
  where
    appendo x y z = Invoke "appendo" [x, y, z]