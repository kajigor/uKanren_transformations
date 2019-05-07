module CpdTest (tests) where

import Bool
import Bridge
import CPD
import Control.Monad
import Data.Maybe
import Data.List
import DotPrinter
import qualified Eval as E
import qualified GlobalControl as GC
import GlobalTreePrinter
import List
import LogicInterpreter
import Num
import Programs
import Purification
import Residualize
import SldTreePrinter
import Syntax
import Text.Printf
import Debug.Trace
import Prelude hiding (succ)
import qualified Data.Set as Set
import CpdResidualization
import System.Directory
import qualified Bottles
import qualified Desert
import qualified Sudoku4x4
import qualified OCanrenize as OC
import Miscellaneous
import Unify
import Path

tests = do
  testEmbedding
  testSelect
  testTakingOutLets
  testPopingOutFreshes
  testNormalization
  testUnifyStuff
  testLocalControl
  testMCS
  testMsgExists
  testSubconjs
  testMinimallyGeneral
  testComplementSubconjs
  testSplit
  testIsGroundTerm
  testGenerateFreshName
  testRenameGoals
  testUnifyInvocationsStuff

  -- printStuff
  testAbstract

  -- printGlobalStuff

  -- littleTest

reportError :: Show a => String -> a -> a -> IO ()
reportError name expected actual =
  putStrLn $ printf "%s failed\nExpected: %s\nActual:   %s" name (show expected) (show actual)

assertCustom :: Show a => String -> (a -> a -> Bool) -> a -> a -> IO ()
assertCustom name check expected actual =
  unless (check expected actual) $ reportError name expected actual

anyAssertCustom :: Show a => String -> (a -> a -> Bool) -> [a] -> a -> IO ()
anyAssertCustom name check expected actual =
  mapM_ (\e -> assertCustom name check e actual) expected

manyAssertCustom :: Show a => String -> (a -> a -> Bool) -> [a] -> [a] -> IO ()
manyAssertCustom name check expected actual =
  unless (all (\a -> isJust $ find (check a) expected) actual) $ putStrLn (printf "%s failed\nExpected: %s\nActual:   %s" name (show expected) (show actual))

assert :: (Show a, Eq a) => String -> a -> a -> IO ()
assert name =
  assertCustom name (==)

manyAssert :: (Show a, Eq a) => String -> a -> (b -> с -> a) -> [(b, с)] -> IO ()
manyAssert name expected f =
  mapM_ (\(x, y) -> assert name expected (f x y))

manyAssertOne :: (Show a, Eq a) => String -> a -> (b -> a) -> [b] -> IO ()
manyAssertOne name expected f =
  mapM_ (assert name expected . f)

printStuff = do
  -- test "l_unify_same" Unify.querySame
  -- test "desert"          Desert.query
  test "path" Path.query1

  -- test "l_unify" Unify.query
  -- test "sldDouble" (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"]))
  -- -- test "simpleDouble" (appendo $ fresh ["x", "y", "z", "t", "r"] (call "appendo" [V "x", V "y", V "t"] &&& call "appendo" [V "t", V "z", V "r"]))
  -- test "sldAppNil"  (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"]))
  -- test "maxLengtho" (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"]))
  -- test "maxo"       (maxo $ fresh ["x", "m"] (call "maxo" [V "x", V "m"]))
  -- test "commute"    (appendo $ fresh ["a", "b", "c"] (call "appendo" [V "a", V "b", V "c"] &&& call "appendo" [V "b", V "a", V "c"]))
  -- test "listo"      (appendo $ listo $ fresh ["a", "b", "c"] (call "listo" [V "a"] &&&
  --                                                             call "listo" [V "b"] &&&
  --                                                             call "listo" [V "c"] &&&
  --                                                             call "appendo" [V "a", V "b", V "c"] &&&
  --                                                             call "appendo" [V "b", V "a", V "c"]))
  -- test "inBotho"  (inBotho $ fresh ["x", "l"] (call "inBotho" [V "x", (C "a" [] % nil), V "l" ]))
  -- test "copycopy" (copycopy $ fresh ["l", "l1", "l2"] (call "copycopy" [V "l", V "l1", V "l2"]))
  --
  -- test "allDiff.dot" Sudoku4x4.queryAllDiff
  where
    test filename goal =
      printTree (printf "%s.dot" filename) $ topLevel goal

doOcanrenize = do
  -- ocanren "pathNat" Path.queryNat $ Just Path.env
  -- ocanren "pathPair" Path.queryPair $ Just Path.env
  -- ocanren "pathPair1" Path.queryPair1 $ Just Path.env
  --
  -- ocanren "pathElem" Path.queryElem $ Just Path.env
  -- ocanren "pathElem1" Path.queryElem1 $ Just Path.env

  ocanren "path" Path.query1 $ Just Path.env


{------------------------------------
  Working examples
-------------------------------------}
  ocanren "appNil"          (doubleAppendo $ fresh ["y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"])) Nothing
  ocanren "double"          (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" $ map V ["x", "y", "z", "r"])) Nothing
  ocanren "revAcco"         (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])) Nothing
  ocanren "check5"          (check5 $ fresh ["x"] (call "check5" [V "x"])) Nothing
  ocanren "checkList5"      (checkList5 $ fresh ["x"] (call "checkList5" [V "x"])) Nothing
  ocanren "checkListOther5" (checkList5' $ fresh ["x"] (call "checkList5" [V "x"])) Nothing
  ocanren "inBotho"         (inBotho $ fresh ["x", "l"] (call "inBotho" [V "x", C "a" [] % nil, V "l" ])) Nothing
  ocanren "maxLengtho"      (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"])) Nothing
  ocanren "maxo"            (maxo $ fresh ["x", "m"] (call "maxo" [V "x", V "m"])) Nothing
{-------------------------------------}


  ocanren "unify" Unify.query $ Just Unify.env
  ocanren "desert"          Desert.query'' $ Just Desert.env
  ocanren "desertSecond"    Desert.query' $ Just Desert.env
  -- ocanren "smallBridge"     (game2 $ fresh ["a", "b"] (call "getAnswer'" [V "a", C "some" [V "b"]])) $ Just Bridge.env
  -- ocanren "bigBridge"       (topLevelBigBridge $ fresh ["a", "b"] (call "tlBigBridge" [V "a", V "b"])) $ Just Bridge.env

  -- ocanren "bottles"         Bottles.query $ Just Bottles.env

  -- ocanren "revAcco" (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])) $ Nothing
  -- ocanren "bottles1"        Bottles.query' $ Just Bottles.env
    where
      ocanren filename goal env = do
        let (tree, logicGoal, names) = GC.topLevel goal
        -- let f = residualizeGlobalTree tree
        -- let pur = purification (f $ vident <$> logicGoal, vident <$> reverse names)
        let f = residualizationTopLevel tree
        let pur = purification (f, vident <$> reverse names)
        OC.topLevel (printf "%s.ml" filename) "topLevel" env pur

doResidualization = do
  purify "membero" $ membero $ fresh ["a"] (call "membero" [V "a", V "a" % nil])
  -- purify "bottles.mk"         Bottles.query
  -- purify "appNil.mk"          (doubleAppendo $ fresh ["y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"]))
  --
  -- purify "maxLengtho.mk"      (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"]))
  -- purify "maxo.mk"            (maxo $ fresh ["x", "m"] (call "maxo" [V "x", V "m"]))
  --
  -- purify "revAcco.mk"         (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"]))
  -- purify "double.mk"          (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" $ map V ["x", "y", "z", "r"]))
  -- purify "appNil.mk"          (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"]))
  -- purify "inBotho.mk"         (inBotho $ fresh ["x", "l"] (call "inBotho" [V "x", C "a" [] % nil, V "l" ]))
  -- purify "smallBridge.mk"     (game2 $ fresh ["a", "b"] (call "getAnswer'" [V "a", C "some" [V "b"]]))
  -- purify "bigBridge.mk"       (topLevelBigBridge $ fresh ["a", "b"] (call "tlBigBridge" [V "a", V "b"]))
  -- purify "check5.mk"          (check5 $ fresh ["x"] (call "check5" [V "x"]))
  -- purify "checkList5.mk"      (checkList5 $ fresh ["x"] (call "checkList5" [V "x"]))
  -- purify "checkListOther5.mk" (checkList5' $ fresh ["x"] (call "checkList5" [V "x"]))
  -- purify "desert.mk"          (Desert.query)
  -- purify "desertSecond.mk"    (Desert.query')
  -- --
  -- -- -- purify "sudoku.mk"          (Sudoku4x4.query)
  -- -- -- purify "sudokuValid.mk"     (Sudoku4x4.queryValid)
  -- -- -- purify "sudokuInvalid.mk"   (Sudoku4x4.queryInvalid)
  -- --
  -- -- -- purify "logicInterpreter.mk" (logic_interpreter $ fresh ["subst", "fml", "res"] (call "check_subst" [V "subst", V "fml", V "res"]))
  -- -- -- purify "commute.mk"         (appendo $ fresh ["a", "b", "c"] (call "appendo" [V "a", V "b", V "c"] &&& call "appendo" [V "b", V "a", V "c"]))
  -- -- -- purify "listo.mk"           (appendo $ listo $ fresh ["a", "b", "c"] (call "listo" [V "a"] &&&
  -- -- --                                                                       call "listo" [V "b"] &&&
  -- -- --                                                                       call "listo" [V "c"] &&&
  -- -- --                                                                       call "appendo" [V "a", V "b", V "c"] &&&
  -- -- --                                                                       call "appendo" [V "b", V "a", V "c"]))

    where
      purify filename goal = do
        let (tree, logicGoal, names) = GC.topLevel goal
        let folder = "residualized"
        createDirectoryIfMissing True folder
        let result = residualizationTopLevel tree
        writeFile (printf "%s/%s" folder filename) (printf "Before:\n%s\n\nAfter:\n%s" (show goal) (show result))


printGlobalStuff = do
  test "global_path" Path.query1
  -- test "unify" Unify.query
  -- test "globalPath" Path.query1

  -- test "globalPath" Path.queryElem1


  -- test "globalDesert"          Desert.query''

  -- test "unify_same" Unify.querySame
  -- test "unify" Unify.query
  -- test "getTerm" Unify.queryGet

  -- test "globalBottles" Bottles.query
  -- test "globalDouble"  (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"]))
  -- test "globalCommute" (appendo $ fresh ["a", "b", "c"] (call "appendo" [V "a", V "b", V "c"] &&& call "appendo" [V "b", V "a", V "c"]))
  -- test "globalAppNil"  (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"]))
  -- test "globalRevAcco" (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"]))
  -- test "globalListo"   (appendo $ listo $ fresh ["a", "b", "c"] (call "listo" [V "a"] &&&
  --                                                                call "listo" [V "b"] &&&
  --                                                                call "listo" [V "c"] &&&
  --                                                                call "appendo" [V "a", V "b", V "c"] &&&
  --                                                                call "appendo" [V "b", V "a", V "c"]))
  -- test "globalInBotho"    (inBotho $ fresh ["x", "l"] (call "inBotho" [V "x", C "a" [] % nil, V "l" ]))
  -- test "globalMaxLengtho" (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"]))
  -- test "globalMaxo"       (maxo $ fresh ["x", "m"] (call "maxo" [V "x", V "m"]))
  -- test "globalSmallBridge"(game2 $ fresh ["a", "b"] (call "getAnswer'" [V "a", C "some" [V "b"]]))
  --
  -- test "globalBigBridge"       (game2Big $ fresh ["a", "b"] (call "result" [V "b"] &&& call "getAnswer" [V "a", C "some" [V "b"]]))
  -- test "globalCheck5"          (check5 $ fresh ["x"] (call "check5" [V "x"]))
  -- test "globalCheckList5"      (checkList5 $ fresh ["x"] (call "checkList5" [V "x"]))
  -- test "globalCheckListOther5" (checkList5' $ fresh ["x"] (call "checkList5" [V "x"]))
  -- -- printTree "logicInterpreter.dot" $ GC.topLevel (logic_interpreter $ fresh ["subst", "fml", "res"] (call "check_subst" [V "subst", V "fml", V "res"]))
  -- -- printTree "check5.dot"    $ topLevel (check5 $ fresh ["x"] (call "check5" [V "x"]))
  -- test "global_copycopy"    (copycopy $ fresh ["l", "l1", "l2"] (call "copycopy" [V "l", V "l1", V "l2"]))
  -- test "globalSimpleDouble" (appendo $ fresh ["x", "y", "z", "t", "r"] (call "appendo" [V "x", V "y", V "t"] &&& call "appendo" [V "t", V "z", V "r"]))
  --
  -- -- test "globalSudoku"  Sudoku4x4.queryInvalid
  -- -- test "globalAllDiff" Sudoku4x4.queryAllDiff
    where
      test fileName goal =
        printTree (printf "%s.dot" fileName) $ fst3 $ GC.topLevel goal


testEmbedding = do
  testHomeo
  testInst
  testStrictInst
  testEmbed
  testVariant
  testRenaming
  where
    testHomeo = do
      testHomeoTerm
      testHomeoGoal
      testHomeoConj
    testInst = do
      testInstTerm
      testInstGoal
      testInstConj
    testStrictInst = do
      testStrictInstTerm
      testStrictInstGoal
      testStrictInstConj
    testEmbed = do
      testEmbedGoal
      testEmbedConj
    testHomeoTerm = do
      manyAssert "homeo term" True homeo  [ (x, y)
                                          , (v, c [v])
                                          , (n [v], n [m [v]])
                                          ]
      manyAssert "homeo term" False homeo [ (c [], v)
                                          , (v, c [])
                                          , (c [v], n [m [v]])
                                          ]
    testHomeoGoal = do
      manyAssert "homeo goal" True homeo  [ (f [], f [])
                                          , (f [v], f [x])
                                          , (f [n [v]], f [m [n [v]]])
                                          , (f [m [v], n [v, v]], f [m [x], n [v, m [v]]])
                                          ]
      manyAssert "homeo goal" False homeo [ (f [], g [])
                                          , (f [c [v]], f [n [m [v]]])
                                          , (f [m [v], n [v, m []]], f [m [], n [v, v]])
                                          ]
    testHomeoConj = do
      manyAssert "homeo conj" True homeo  [ ([f []], [f []])
                                          , ([f [], g[]], [f [], f [], g []])
                                          , ([f [v]], [f [x]])
                                          , ([f [n [v]]], [f [m [n [v]]]])
                                          , ([f [m [v], n [v, v]]], [f [m [x], n [v, m [v]]]])
                                          ]
      manyAssert "homeo conj" False homeo [ ([f []], [g []])
                                          , ([f [c [v]]], [f [n [m [v]]]])
                                          ]
    testInstTerm = do
      manyAssert "isInst term" True isInst  [ (v, x)
                                            , (v, n [])
                                            , (n [], n [])
                                            , (n [x, y], n [y, x])
                                            , (n [x, y], n [x, x])
                                            , (n [x, m [x]], n [y, m [y]])
                                            ]

      manyAssert "isInst term" False isInst [ (n [], v)
                                            , (n [v], n [])
                                            , (n [x, x], n [y, x])
                                            , (n [x, m [x]], n [x, y])
                                            , (n [x, m [x]], n [x, m [y]])
                                            ]

    testInstGoal = do
      manyAssert "isInst goal" True isInst  [ (f [v], f [n [x]])
                                            , (f [x, y], f [y, x])
                                            , (f [x, y], f [x, x])
                                            ]

      manyAssert "isInst goal" False isInst [ (f [], g [])
                                            , (f [n [x, m [x]]], f [n [x, m [y]]])
                                            , (f [], f [v])
                                            , (f [v], f [])
                                            ]
    testInstConj = do
      manyAssert "isInst conj" True isInst  [ ([f [v]], [f [n [x]]])
                                            , ([f [x, y], g [v]], [f [x, x], g [x]])
                                            , ([f [x, y], g [y]], [f [x, x], g [x]])
                                            ]

      manyAssert "isInst conj" False isInst [ ([], [f []])
                                            , ([f [n [x, m [x]]]], [f [n [x, m [y]]]])
                                            , ([f [x, y], g [x]], [f [x, x], g [y]])
                                            ]


    testStrictInstTerm = do
      manyAssert "isStrictInst term" True isStrictInst  [ (v, n [])
                                                        , (n [x, y], n [x, x])
                                                        ]

      manyAssert "isStrictInst term" False isStrictInst [ (v, x)
                                                        , (n [], v)
                                                        , (n [], n [])
                                                        , (n [v], n [])
                                                        , (n [x, y], n [y, x])
                                                        , (n [x, x], n [y, x])
                                                        , (n [x, m [x]], n [y, m [y]])
                                                        , (n [x, m [x]], n [x, y])
                                                        , (n [x, m [x]], n [x, m [y]])
                                                        ]

    testStrictInstGoal = do
      manyAssert "isStrictInst goal" True isStrictInst  [ (f [v], f [n [x]])
                                                        , (f [x, y], f [x, x])
                                                        ]

      manyAssert "isStrictInst goal" False isStrictInst [ (f [], g [])
                                                        , (f [n [x, m [x]]], f [n [x, m [y]]])
                                                        , (f [], f [v])
                                                        , (f [v], f [])
                                                        , (f [x, y], f [y, x])
                                                        ]
    testStrictInstConj = do
      manyAssert "isStrictInst conj" True isStrictInst  [ ([f [v]], [f [n [x]]])
                                                        , ([f [x, y], g [v]], [f [x, x], g [x]])
                                                        , ([f [x, y], g [y]], [f [x, x], g [x]])
                                                        , ([f [x, y]], [f [x, x]])
                                                        ]

      manyAssert "isStrictInst conj" False isStrictInst [ ([], [f []])
                                                        , ([f [n [x, m [x]]]], [f [n [x, m [y]]]])
                                                        , ([f [x, y], g [x]], [f [x, x], g [y]])
                                                        ]



    testEmbedGoal = do
      manyAssert "embed goal" False embed [ (f [n [x, v], x, y], f [v, x, y])
                                          , (f [x, x], f [x, y])
                                          , (f [x, x, y, y], f [x, x, y, z])
                                          ]

      manyAssert "embed goal" True embed  [ (f [x, y], f [x, x])
                                          , (f [x, x, y, y], f [x, z, z, x])
                                          , (f [x, z, z, x], f [x, x, y, z])
                                          , (f [v, x, x], f [n [x, v], x, y])
                                          , (f [v, x, n [y, x]], f [v, x, n [y, x]])
                                          , (f [x, y, x], f [x, y, y])
                                          , (f [x, y, y], f [x, y, x])
                                          , (f [v, x, y], f [x, y, v]) -- variant
                                          , (f [v, x, y], f [n [x, v], x, y]) -- not a strict instance
                                          , (f [c [], m [x, x]], f [n [c []], m [x, y]])
                                          , (maxo1 v15 (s (s v20)) v1, maxo1 v50 (s (s v51)) v1)
                                          ]
    testEmbedConj = do
      manyAssert "embed conj" False embed [ ([f [n [x, v], x, y]], [f [v, x, y]])
                                          , ([f [x, x]], [f [x, y]])
                                          , ([f [x, x, y, y]], [f [x, x, y, z]])
                                          , ([f [x, y], g []], [f [x, x]])
                                          , ([g [], f [x, y]], [f [x, x]])
                                          , ([f [x, x], f [x, x]], [f [x, x]])
                                          , ([f [x, y], f [x, y]], [f [x, x]])
                                          ]

      manyAssert "embed conj" True embed  [ ([f [x, y]], [f [x, x]])
                                          , ([f [x, y]], [g [x], f [x, x]])
                                          , ([f [x, y]], [g [], f [x, x], g []])
                                          , ([f [x, x, y, y]], [f [x, z, z, x]])
                                          , ([f [x, z, z, x]], [f [x, x, y, z]])
                                          , ([f [v, x, x]], [f [n [x, v], x, y]])
                                          , ([f [x, x, y, y], f [x, z, z, x], f [v, x, x]], [f [x, z, z, x], g [], g[], f [x, x, y, z], f [n [x, v], x, y]])
                                          , ([f [v, x, n [y, x]]], [f [v, x, n [y, x]]])
                                          , ([f [x, y, x]], [f [x, y, y]])
                                          , ([f [x, y, y]], [f [x, y, x]])
                                          , ([f [v, x, y]], [f [x, y, v]]) -- variant
                                          , ([f [v, x, y]], [f [n [x, v], x, y]]) -- not a strict instance
                                          , ([f [c [], m [x, x]]], [f [n [c []], m [x, y]]])
                                          ]
    testVariant = do -- TODO more  tests
      manyAssert "variant goal" False isVariant [ (f [x, y], f [x, x])
                                                , (f [x, x], f [x, y])
                                                ]
      manyAssert "variant goal" False isVariant [ (app v19 (cons v18 (cons v12 nil)) (cons v12 (cons v18 v19)),
                                                   app v22 (cons v18 (cons v21 nil)) (cons v18 (cons v21 v22)))
                                                ]
      where
        app x y xy = Invoke "appendo" [x, y, xy]
        v12 = V 12
        v19 = V 19
        v18 = V 18
        v21 = V 21
        v22 = V 22
        nil = C "Nil" []
        cons h t = C "Cons" [h, t]
    testRenaming = do -- TODO more tests
      manyAssert "renaming goal" True  isRenaming [ (f [x, y], f [x, x])
                                                  ]
      manyAssert "renaming goal" False isRenaming [ (f [x, x], f [x, y])
                                                  ]

    x = V "x"
    y = V "y"
    z = V "z"
    v = V ""
    c = C ""
    n = C "n"
    m = C "m"
    f = Invoke "f"
    g = Invoke "g"
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    v1 = V "1"
    v15 = V "15"
    v20 = V "20"
    v50 = V "50"
    v51 = V "51"
    s x = C "s" [x]

checkEmbed =
  embed (Invoke "all_Ch" [V 19, V 21, (C ":" [(C "Some" [V 20, V 19]), V 15]), C "true" []])
        (Invoke "all_Ch" [V 86, V 88, (C ":" [(C "Some" [V 20, V 73]), V 15]), C "true" []])

checkEmbed' =
  embed (Invoke "all_Ch" [V 19, V 21, (C ":" [(C "Some" [V 20, V 19]), V 15]), C "true" []])
        (Invoke "all_Ch" [V 79, V 81, (C ":" [(C "Some" [V 20, V 73]), V 15]), C "true" []])

checkEmbed'' =
  embed [Invoke "all_Ch" [V 19, V 21, (C ":" [(C "Some" [V 20, V 19]), V 15]), C "true" []]]
        [(Invoke "all_Ch" [V 86, V 88, (C ":" [(C "Some" [V 20, V 73]), V 15]), C "true" []])
        ,(Invoke "all_Ch" [V 79, V 81, (C ":" [(C "Some" [V 20, V 73]), V 15]), C "true" []])]


testSelect = do
  testSelect1
  testSelect2
  where
    testSelect1 = do
      assert "select 0" (Just app00D) (select [app00D, app01D])
      assert "select 1" (Just app11D) (select [app10D, app11D])
      assert "select 2" (Just app21D) (select [app20D, app21D])
      where
        xs  = V 0
        ys  = V 1
        t   = V 2
        zs  = V 3
        r   = V 4
        xs' = V 5
        t'  = V 6
        h   = V 7
        r'  = V 8
        cons h t = C "cons" [h, t]
        app  x y z = Invoke "app"  [x, y, z]
        app00 = app xs ys t
        app01 = app t zs r
        app00D = Descend app00 Set.empty
        app01D = Descend app01 Set.empty
        app10 = app xs' ys t'
        app11 = app (cons h t') zs r
        app10D = Descend app10 $ Set.singleton app00
        app11D = Descend app11 Set.empty
        app20 = app10
        app21 = app t' zs r'
        app20D = Descend app20 $ Set.singleton app00
        app21D = Descend app21 $ Set.singleton app11
    testSelect2 = do
      assert "select 0" (Just max0D) (select [max0D, len0D])
      assert "select 1" (Just len1D) (select [max1D, len1D])
      assert "select 2" (Just len2D) (select [max2D, len2D])
      where
        n = V 0
        m = V 1
        l = V 2
        h = V 3
        k = V 4
        t = V 5
        x = V 6
        y = V 7
        z = V 8
        cons h t = C "Cons" [h, t]
        max' x y z = Invoke "max'" [x, y, z]
        len  x y   = Invoke "len"  [x, y]
        max0 = max' x n m
        len0 = len x l
        max0D = Descend max0 Set.empty
        len0D = Descend len0 Set.empty
        max1 = max' t n m
        len1 = len (cons h t) l
        max1D = Descend max1 $ Set.singleton max0
        len1D = Descend len1 Set.empty
        max2 = max' t n m
        len2 = len t k
        max2D = Descend max2 $ Set.singleton max0
        len2D = Descend len2 $ Set.singleton len1

testTakingOutLets = do
  assert "taking out lets 0" (uni0, [], []) (justTakeOutLets (uni0, []))
  assert "taking out lets 1" (uni1, [], [(defName, args0', uni0')]) (justTakeOutLets (flatlet, []))
  assert "taking out lets 2" (conj, [], [(defName, args0'', uni0''), (fedName, args2', disj')]) (justTakeOutLets (doublet, []))
  where
    x = V "x"
    y = V "y"
    z = V "z"
    y0 = V "y0"
    y1 = V "y1"
    y2 = V "y2"
    y3 = V "y3"
    y4 = V "y4"
    uni0 = x === y
    uni1 = y === z
    uni2 = x === z
    conj = uni0 &&& uni1
    disj = conj ||| uni2
    uni0' = y0 === y1
    uni0'' = y3 === y4
    uni1' = y1 === y2
    uni2' = y0 === y2
    conj' = uni0' &&& uni1'
    disj' = conj' ||| uni2'
    defName = "def"
    fedName = "fed"
    args0 = []
    args1 = ["x"]
    args0'  = ["y0", "y1"]
    args0'' = ["y3", "y4"]
    args1' = ["y2"]
    args2' = ["y1", "y2", "y0"]
    flatlet = Let (def defName args0 uni0) uni1
    doublet = Let (def defName args0 uni0) (Let (def fedName args1 disj) conj)

testPopingOutFreshes = do
  assert "popping freshes up 0" (callF x' y') (fst3 $ E.preEval' E.env0 (fresh ["x", "y"] goal))
  assert "popping freshes up 1" (fLet (callF x' y')) (fst3 $ E.preEval' E.env0 (fresh ["x", "y"] $ fLet goal))
  assert "popping freshes up 2" (body', reverse [0..4]) ((\(x, _, y) -> (x, y)) $ E.preEval' E.env0 (fresh ["m", "n"] body))
  where
    x = V "x"
    y = V "y"
    m = V "m"
    n = V "n"
    h = V "h"
    t = V "t"
    cS x = C "S" [x]
    cT x = C "T" [x]
    callF x y = Invoke "f" [x, y]
    callT x = Invoke "t" [x]
    gamma = []
    goal = callF x y
    body = fresh ["h"] (m === cS h &&& fresh ["t"] (n === cT t) &&& m === cS n) ||| fresh ["h"] (m === n &&& m === cT h &&& callT h)
    fLet = Let (def "f" ["m", "n"] body)
    x' = V 0
    y' = V 1
    body' = (V 0 === cS (V 2) &&& (V 1 === cT (V 3) &&& V 0 === cS (V 1))) ||| (V 0 === V 1 &&& (V 0 === cT (V 4) &&& callT (V 4)))

testNormalization = do
  assert "normalization 0" [[t === u]] (normalize (t === u))
  assert "normalization 1" [[f]] (normalize f)
  assert "normalization 2" [[f, g]] (normalize (f &&& g))
  assert "normalization 3" [[f], [g]] (normalize (f ||| g))
  assert "normalization 4" [[m], [f, g, t === u], [h, t === u]] (normalize (m ||| (f &&& g ||| h) &&& (t === u) ))
  where
    x = V 0
    y = V 1
    t = V 42
    u = V 13
    f = Invoke "f" []
    g = Invoke "g" []
    h = Invoke "h" []
    m = Invoke "m" []

testUnifyStuff = do
  assert "unifyStuff 0" (Just ([], E.s0)) (unifyStuff' [])
  assert "unifyStuff 1" (Just ([], [(0, y)])) (unifyStuff' [x === y, y === x])
  assert "unifyStuff 2" (Just ([], [(1, s), (0, y)])) (unifyStuff' [x === y, x === s])
  assert "unifyStuff 3" Nothing (unifyStuff' [x === s, x === t])
  assert "unifyStuff 4" Nothing (do (gs, state) <- unifyStuff' [x === s]
                                    unifyStuff state [x === t]
                                )
  assert "unifyStuff 5" (Just ([f x y, g x], [(13, x), (1, t), (0, y)])) (unifyStuff' [f x y, x === y, g x, t === y, x === u])
  assert "unifyStuff 6" (Just (inp, E.s0)) (unifyStuff' inp)
  where
    unifyStuff' = unifyStuff E.s0
    inp = [f x y, f u v, g x, g v, g s]
    x = V 0
    y = V 1
    u = V 13
    v = V 42
    s = C "s" []
    t = C "t" []
    f x y = Invoke "f" [x, y]
    g x = Invoke "g" [x]
    h = Invoke "h" []

testLocalControl = do
  manyAssertCustom "local control" isVariant [[app x y z], [app x y t, app t z r]] (leaves $ topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])))
  manyAssertCustom "local control" isVariant [[app x y z]] (leaves $ topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"])))
  assertCustom "local control" (\x y -> length x == length y) [] (leaves $ topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [C "Stupid" [], V "y", V "z", V "r"])))
  manyAssertCustom "local control" isVariant [ [ maxo1 x zero z, lengtho x y ]
                                             , [ maxo1 (x % y) (succ z) r, lengtho y t ]
                                             , [ leo z u trueo, maxo1 (x % y) (succ (succ u)) r, lengtho y t ]
                                             , [ maxo1 (x % y) (succ (succ r)) t, lengtho y z]
                                             , [ gto z r trueo, maxo1 (x % y) (succ (succ z)) t, lengtho y u ]
                                             ]
                                             (leaves $ topLevel (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"])))
  where
    app x y z = Invoke "appendo" [x, y, z]
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    lengtho x y = Invoke "lengtho" [x, y]
    gto x y z = Invoke "gto" [x, y, z]
    leo x y z = Invoke "leo" [x, y, z]
    x = V 0
    y = V 1
    t = V 2
    z = V 3
    r = V 4
    u = V 5

testMCS = do
  assert "mcs 0" [[p x y, q x], [p t u], [q v]] (mcs [p x y, p t u, q x, q v])
  assert "mcs 1" [] (mcs ([] :: [G X]))
  assert "mcs 2" [[p x y, p x z, p y z]] (mcs [p x y, p x z, p y z])
  assert "mcs 3" [[p x y, p x y, p x y, p z y, q x, q y], [p t u, p u v, p v t]] (mcs [p x y, p x y, p x y, p z y, q x, p t u, p u v, p v t, q y])
  where
    p x y = Invoke "p" [x, y]
    q x = Invoke "q" [x]
    x = V "x"
    y = V "y"
    z = V "z"
    t = V "t"
    u = V "u"
    v = V "v"

testMsgExists = do
  manyAssert "MSG exists" False msgExists [ ([], [f])
                                          , ([f], [])
                                          , ([f, f], [f, g])
                                          , ([f, g, f], [f, f, g])
                                          , ([f], [g])
                                          ]
  manyAssert "MSG exists" True  msgExists [ ([h (m x y) x, h y x], [h x y, h (n (m x y)) z])
                                          , ([f, g], [f, g])
                                          , ([f], [f])
                                          , ([], [])
                                          ]
  where
    f = Invoke "f" []
    g = Invoke "g" []
    h x y = Invoke "h" [x, y]
    k x = Invoke "k" [x]
    true = C "True" []
    false = C "False" []
    m x y = C "m" [x, y]
    n x = C "n" [x]
    x = V "x"
    y = V "y"
    z = V "z"

testSubconjs = do
  assert "subconjs 0" 3  (length $ subconjs [f, g, f] 2)
  assert "subconjs 1" 10 (length $ subconjs [f, g, f, f, f] 3)
  assert "subconjs 2" 0  (length $ subconjs [f] 2)
  assert "subconjs 3" True (all (\x -> x `elem` [ [f, g, l]
                                                , [f, g, f]
                                                , [f, g, p]
                                                , [f, l, f]
                                                , [f, l, p]
                                                , [f, f, p]
                                                , [g, l, f]
                                                , [g, l, p]
                                                , [g, f, p]
                                                , [l, f, p]
                                                ]) (subconjs [f, g, l, f, p] 3))
  where
    f = Invoke "f" []
    g = Invoke "g" []
    h x y = Invoke "h" [x, y]
    x = V "x"
    y = V "y"
    m x = C "m" [x]
    l = h x y
    p = h (m x) x

testComplementSubconjs = do
  assert "complement subconjuncions 0" [] (complementSubconjs [f, g, l, f, p] [f, g, l, f, p])
  assert "complement subconjuncions 1" [f, p] (complementSubconjs [f, g, l] [f, g, l, f, p])
  assert "complement subconjuncions 2" [l, p] (complementSubconjs [f, g, f] [f, g, l, f, p])
  assert "complement subconjuncions 3" [l, f] (complementSubconjs [f, g, p] [f, g, l, f, p])
  assert "complement subconjuncions 4" [g, p] (complementSubconjs [f, l, f] [f, g, l, f, p])
  assert "complement subconjuncions 5" [g, f] (complementSubconjs [f, l, p] [f, g, l, f, p])
  assert "complement subconjuncions 6" [g, l] (complementSubconjs [f, f, p] [f, g, l, f, p])
  assert "complement subconjuncions 7" [f, f] (complementSubconjs [g, l, p] [f, g, l, f, p])
  assert "complement subconjuncions 8" [f, l] (complementSubconjs [g, f, p] [f, g, l, f, p])
  assert "complement subconjuncions 9" [f, g] (complementSubconjs [l, f, p] [f, g, l, f, p])
  where
    f = Invoke "f" []
    g = Invoke "g" []
    h x y = Invoke "h" [x, y]
    x = V "x"
    y = V "y"
    m x = C "m" [x]
    l = h x y
    p = h (m x) x

testMinimallyGeneral = do
  assert "minimally general 0" [f x x] (minimallyGeneral [[f x y], [f x x]])
  assert "minimally general 1" [f x x] (minimallyGeneral [[f x x], [f x y]])
  assert "minimally general 2" [g x x y]  (minimallyGeneral [[g x x y], [g x y y], [g x y x], [g x y z]])
  assert "minimally general 3" [g x y y]  (minimallyGeneral [[g x y z], [g x y y], [g x y x], [g x x y]])
  assert "minimally general 4" [f x y, g x y z] (minimallyGeneral [[f x y, g x y z], [f x z], [f x x], [f x x, g x y z]]) -- y and x are linked and the selected one is the first
  assert "minimally general 5" [f x x] (minimallyGeneral [[f x z], [f x x], [f x x, g y y z], [f x y, g z t u]])
  assert "minimally general 6" [f x z, g x y z] (minimallyGeneral [[f x z, g x y z], [f x x, g y y z], [f x y, g z t u]])
  assert "minimally general 7" [f x x, g y y z] (minimallyGeneral [[f x x, g y y z], [f x y, g z t u], [f x z, g x y z]])
  assert "minimally general 8" [p u y, q y z] (minimallyGeneral [[p u y, q y z], [p x u, q y z]])
  assert "minimally general 9" [p u y, q y z] (minimallyGeneral [[p x u, q y z], [p u y, q y z]])
  where
    f x y = Invoke "f" [x, y]
    g x y z = Invoke "g" [x, y, z]
    p x y = Invoke "p" [x, y]
    q x y = Invoke "q" [x, y]
    x = V "x"
    y = V "y"
    z = V "z"
    t = V "t"
    u = V "u"

testSplit = do -- TODO more tests
  assertCustom "split 0" checkVariant ([f x x], [g x]) (fst $ split [2..] [f x x] [f x x, g x] )
  assertCustom "split 1" checkVariant ([f x x], [g x]) (fst $ split [2..] [f x x] [g x, f x x] )
  assertCustom "split 2" checkVariant ([f x z], [g x]) (fst $ split [2..] [f x y] [g x, f x x] )
  assertCustom "split 3" checkVariant ([maxo1 (v150 % v153) (s v152) v1, lengtho v153 v154], [leo v121 v122])
                                      (fst $
                                       split [150..]
                                             [maxo1 (v51 % v52) (s v33) v1, lengtho v52 v53]
                                             [leo v121 v122, maxo1 (v126 % v127) (s (s (s (s (s v122))))) v1, lengtho v127 v128])
  assertCustom "split 4" checkVariant ([maxo1 v50 (s (s v51)) v1], [])
                                      (fst $
                                       split [50..] [maxo1 v15 (s (s v20)) v1] [maxo1 v50 (s (s v51)) v1])
  where
    checkVariant (x, x') (y, y') = isVariant x y && isVariant x' y'

    x = V 0
    y = V 1
    z = V 2
    f x y = Invoke "f" [x, y]
    g x = Invoke "g" [x]
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    leo x y = Invoke "leo" [x, y, trueo]
    lengtho x y = Invoke "lengtho" [x, y]
    s x = C "S" [x]
    v1  = V 1
    v15 = V 15
    v20 = V 20
    v33 = V 33
    v50 = V 50
    v51 = V 51
    v52 = V 52
    v53 = V 53
    v121 = V 121
    v122 = V 122
    v126 = V 126
    v127 = V 127
    v128 = V 128
    v150 = V 150
    v151 = V 151
    v152 = V 152
    v153 = V 153
    v154 = V 154

testAbstract = do
  assert "abstract" [goal] (map fst $ fst $ GC.abstract (Descend goal Set.empty) goal [11..])
  where
    goal = [maxo1 v3 zero v1]
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    v3 = V 3
    v1 = V 1
    zero = C "O" []

littleTest = do
  manyAssert "embed conj1" False embed [ ( [getAnswer' [v4,   st [false, true, false, true, false], some [v8]],  add [v10,v8, s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                                         , [getAnswer' [v140, st [false, true, false, true, false], some [v144]],add [v10,v77,s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                                         )
                                       ]
  manyAssert "embed conj2" True  embed [ ( [getAnswer' [v4,   st [false, true, false, true, false], some [v8]],  add [v10,v8,s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                                         , [getAnswer' [v140, st [false, true, false, true, false], some [v144]],add [v146,v144,v145], add [v78,v145,v77],add [v10,v77,s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                                         )
                                       ]
  where
    v4 = V 4
    v8 = V 8
    v9 = V 9
    v10 = V 10
    v77 = V 77
    v78 = V 78
    v140 = V 140
    v144 = V 144
    v145 = V 145
    v146 = V 146
    st = C "st"
    some = C "some"
    false = C "false" []
    true = C "true" []
    s x = C "s" [x]
    o = C "o" []
    getAnswer' x = Invoke "ga" x
    add x = Invoke "a" x

testIsGroundTerm = do
  manyAssertOne "isGroundTerm" False isGroundTerm [ V ""
                                                  , C "" [V ""]
                                                  , (C "" [C "" [V ""], C "" [C "" [V ""]]])
                                                  , (C "" [C "" [V ""], C "" [C "" []]])
                                                  , (C "" [C "" [], C "" [C "" [V ""]]])
                                                  ]

  manyAssertOne "isGroundTerm" True  isGroundTerm [ C "" []
                                                  , C "a" [C "b" []]
                                                  , (C "" [C "" [], C "" [C "" []]])
                                                  ]

testGetVars = do
  test [x] (v x)
  test [x] [v x, c [v x]]
  test [x, y] [v x, c [v x], d [v x, c [v y, v x]]]
  test [z] (f [c [v z, v z]])
  test [x, y, z] [f [c [v x], d [c [v y]]], g [v x, v z], g [c [v y], d [v z]]]
  where
    test expVars input =
      assert "getVars" (Set.fromList expVars) $ getVars input
    inv = Invoke
    f = inv "f"
    g = inv "g"
    c = C "c"
    d = C "d"
    v = V
    x = "x"
    y = "y"
    z = "z"

testGenerateFreshName = do
  test "f"   []                       "f"
  test "f"   ["g"]                    "f"
  test "f"   ["g", "_f", "abc"]       "f"
  test "_f"  ["_f"]                   "__f"
  test "f"   ["g", "_f", "f", "___f"] "__f"
  test "__f" ["g", "c", "abc"]        "__f"
  where
    test inputName names newName =
      let setNames = Set.fromList names in
      assert "generateFreshName" newName (generateFreshName inputName setNames)


testRenameGoals = do
  test [f [x], g [y]] [] "fG" [x, y]
  test [f [x], g [y]] ["f", "g"] "fG" [x, y]
  test [f [x], g [y]] ["fG"] "_fG" [x, y]
  test [f [x], g [y, y]] [] "fG" [x, y]
  test [f [x, y], g [y, y]] [] "fG" [x, y]
  where
    test goals names newName args =
      let setNames = map (\x -> ([], x, [])) names in
      let insertName name defs = (goals, name, args) : defs in
      assert "renameGoals"
             (insertName newName setNames, newName, args)
             (renameGoals goals setNames)
    inv n args = Invoke n $ map V args
    f = inv "f"
    g = inv "g"
    x = 0
    y = 1

testUnifyInvocationsStuff = do
  test [] [] $ Just []
  test [f []] [f []] $ Just []
  test [f [x]] [f [c [y,z]]] $ Just [(0, c [y,z])]
  test [f [c [x],     y],     g [y,     c [d [x]]]]
       [f [c [c [z]], d [z]], g [d [z], c [d [c [z]]]]] $
       Just [(1, d [z]), (0, c [z])]
  test [f [x]] [f [c [x,x]]] $ Just [(0, c [x, x])]

  test [] [f []] Nothing
  test [f []] [g []] Nothing
  test [f []] [f [x]] Nothing
  -- test [f [x]] [f [c [x,x]]] Nothing
  -- test [f [c [x],     y], g [y,     c [d [x]]]]
  --      [f [c [c [z]], z], g [d [z], c [d [c [z]]]]]
  --      Nothing
  where
    test gs hs expected = assert "unifyInvocationLists" expected (unifyInvocationLists gs hs $ Just E.s0)
    f = Invoke "f"
    g = Invoke "g"
    x = V 0
    y = V 1
    z = V 2
    c = C "c"
    d = C "d"
    e = C "e"
