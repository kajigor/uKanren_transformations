module Test.NonConjunctive  where

import           Test.Helper (test, test2, manyAssert)

import           Printer.Dot
import           Printer.NCTree   ()
import           Program.List     (nil, revAcco, reverso, (%), maxLengtho)
import           Program.Programs (doubleAppendo)
import qualified Program.Prop
import           Program.Stlc     (evalo)
import           Syntax
import           System.Directory
import           System.Process   (system)
import           Text.Printf
import qualified NonConjunctive.Unfold as NC

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
prop = Program.Prop.query3
maxLen = Program maxLengtho $ fresh ["xs", "m", "l"] (call "maxLengtho" [V "xs", V "m", V "l"])
lambda = Program evalo $ fresh ["m", "n"] (call "evalo" [V "m", V "n"])

unit_nonConjunctiveTest = do
  -- runTest NC.nonConjunctive "da" dA
  -- runTest NC.nonConjunctive "rev" rev
  -- runTest NC.nonConjunctive "revAcco" revAcco'
  -- runTest NC.nonConjunctive "prop" prop
  -- runTest NC.nonConjunctive "maxLen" maxLen
  runTest NC.nonConjunctive "lambda" lambda

runTest function filename goal = do
  let (tree, logicGoal, names) = function goal
  let path = printf "test/out/nc/%s" filename
  exists <- doesDirectoryExist path
  if exists
  then removeDirectoryRecursive path
  else return ()
  createDirectoryIfMissing True path
  printTree (printf "%s/tree.dot" path) tree
  system (printf "dot -O -Tpdf %s/*.dot" path)
  return ()

unit_isConflicting = do
  test2 NC.isConflicting [] [] False
  test2 NC.isConflicting [(1, V 2)] [(2, V 3)] False
  test2 NC.isConflicting [(1, V 2)] [(1, V 2)] False
  test2 NC.isConflicting [(1, V 4)] [(1, V 3), (3, V 4)] False
  test2 NC.isConflicting [(1, V 2)] [(1, V 2)] False
  test2 NC.isConflicting [(1, V 2)] [(1, V 3)] False
  test2 NC.isConflicting [(1, V 2)] [(1, C "" [])] False
  test2 NC.isConflicting [(1, V 9),  (6, nil), (7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)]
                         [(2, V 10), (9, nil), (7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)]
                         False
  test2 NC.isConflicting [(1, V 4), (0, nil)] [(2, V 3), (4, nil)] False
  test2 NC.isConflicting [(1, V 4), (0, nil)] [(3, V 8 % V 10), (4, V 8 % V 9)] False
  test2 NC.isConflicting [(4, V 5 % V 7), (0, V 5 % V 6)] [(3, V 8 % V 10), (4, V 8 % V 9)] False

  test2 NC.isConflicting [(4, V 5 % V 7), (0, V 5 % V 6)] [(2, V 3), (4, nil)] True
  test2 NC.isConflicting [(1, C "A" [])] [(1, C "B" [])] True
  test2 NC.isConflicting [(1, C "A" [C "A" []])] [(1, C "A" [C "B" []])] True
  test2 NC.isConflicting [(1, C "A" [])] [(1, C "A" [C "B" []])] True

unit_findConflicting = do
    test NC.findConflicting
         [[(1, V 2)], [(1, a)]]
         [[[(1, V 2)]], [[(1, a)]]]
    test NC.findConflicting
         [[(1, V 2)], [(1, a)], [(2, V 3)]]
         [[[(1, V 2)]], [[(1, a)]], [[(2, V 3)]]]
    test NC.findConflicting
         [[(1, V 2)], [(1, a), (2, b)], [(2, V 3)]]
         [[[(1, V 2)]], [[(1, a), (2, b)]], [[(2, V 3)]]]
    test NC.findConflicting
         [[(1, b)], [(1, a), (2, b)], [(2, V 3)]]
         [[[(1, b)], [(1, a), (2, b)]], [[(2, V 3)]]]
    test NC.findConflicting
         [[(1, b)], [(1, a), (2, b)], [(2, b)]]
         [[[(1, b)], [(1, a), (2, b)]], [[(2, b)]]]
    test NC.findConflicting
         [[(1, b)], [(1, a), (2, b)], [(2, a)]]
         [[[(1, b)], [(1, a), (2, b)], [(2, a)]]]
    test NC.findConflicting
         [[(1, b)], [(1, a), (2, c b)], [(2, c a)]]
         [[[(1, b)], [(1, a), (2, c b)], [(2, c a)]]]
    test NC.findConflicting
         [[(1, V 9),(6, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)], [(2, V 10),(9, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]]
         [[[(1, V 9),(6, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]], [[(2, V 10),(9, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]]]

  where
    a = C "a" []
    b = C "b" []
    c x = C "c" [x]

unit_productList = do
    test NC.productList [[]] ([] :: [[Int]])
    test NC.productList ([] :: [[Int]]) [[]]
    test NC.productList [[1,2,3]] [[1], [2], [3]]
    test NC.productList [[1,2,3], [4], [5,6]] [[1,4,5], [1,4,6], [2,4,5], [2,4,6], [3,4,5], [3,4,6]]
    test NC.productList [ [ ([], [(1, V 4), (0, nil)])
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



unit_unifySubsts = do
  test NC.unifySubsts [] (Just [])
  test NC.unifySubsts [ [(1, V 4), (0, nil)]
                      , [(2, V 3), (4, nil)]
                      ]
                      (Just [(4, nil), (2, V 3), (1, V 4), (0, nil)])
  test NC.unifySubsts [ [(4, V 5 % V 7),  (0, V 5 % V 6)]
                      , [(3, V 8 % V 10), (4, V 8 % V 9)]
                      ]
                      (Just [(7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)])

unit_selectMin = do
  test NC.selectMin [(0,0)] ([], (0,0), [])
  test NC.selectMin [(0,0), (1,1), (2,2)] ([], (0,0), [(1,1), (2,2)])
  test NC.selectMin [(2,2), (1,1), (0,0)] ([(2,2), (1,1)], (0,0), [])
  test NC.selectMin [(1,2), (2,3), (3,0), (4,0), (5,2)] ([(1,2),(2,3)],(3,0),[(4,0),(5,2)])