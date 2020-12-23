{-# LANGUAGE BangPatterns #-}

module Main (main, tests) where

import           Control.Monad
import qualified CPD.GlobalControl        as GC
import           CPD.LocalControl
import           CPD.Residualization
import           Data.Foldable            (for_)
import           Data.List
import           Data.Maybe
import           Debug.Trace
import qualified OCanrenize               as OC
import           Prelude                  hiding (succ)
import           Printer.Dot
import           Printer.GlobalTree ()
import           Printer.SldTree ()
import qualified Program.Bottles
import           Program.List
import           Program.Programs
import           Program.Prop
import           Program.Unify
import           Purification
import           Residualization
import           Syntax
import           System.Directory
import           Text.Printf
import           Util.ConjRetriever
import           Util.Miscellaneous
import           Embed
import           System.CPUTime
import           System.Process           (system)


main :: IO ()
main = do
  doOcanrenize
  -- testUnifySubsts

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


tests = do
  -- printGlobalStuff
  doOcanrenize
  {-
  testEmbedding
  testGround
  testSelect
  testTakingOutLets
  testPopingOutFreshes
  testNormalization
  testUnifyStuff
  -- testLocalControl
  testMCS
  testMsgExists
  testSubconjs
  testMinimallyGeneral
  testComplementSubconjs
  testSplit
  testIsGroundTerm
  testGenerateFreshName
  testRenameGoals
  -- testUnifyInvocationsStuff

  -- printStuff
  testAbstract

  -- printGlobalStuff

  -- littleTest
  -}

printStuff = do
  test "plainEvalo" Program.Prop.plainQuery
  -- test "propInst" Prop.query''

  -- test "plainEvaloConj" Prop.plainQueryConj

  -- test "fAndS" Sample1.query
  -- test "l_unify_same" Unify.querySame
  -- test "desert"          Desert.query
  -- test "path" Path.query1

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
    test filename goal = do
      let path = "test/out/" ++ filename
      createDirectoryIfMissing True path
      printTree (printf "%s/local.dot" path) $ topLevel goal Deterministic

doOcanrenize = do
  ocanren "unify" Program.Unify.query $ Just Program.Unify.env

  -- ocanren "isPath" Program.Path.query1 $ Just Program.Path.env


  ocanren "prop__" Program.Prop.query'' $ Nothing
  ocanren "propPalin__" Program.Prop.plainQuery Nothing

  ocanren "bottles" Program.Bottles.query $ Nothing

  ocanren "doubleAppendo" (Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])) Nothing


  -- ocanren "specialProp" Program.SpecialProp.logintoQuery Nothing

  -- ocanren "plainEvalo" Program.Prop.plainQuery Nothing

  -- ocanren "doubleRev" (Program doubleReverso $ fresh ["xs"] (call "doubleReverso" [V "xs"])) Nothing


  -- ocanren "fun" (fun $ fresh ["n", "x", "r"] (call "fun" $ map V ["n", "x", "r"])) Nothing

  -- ocanren "oddo" (oddo $ (call "oddo" [zero])) Nothing

  -- ocanren "propInst" Prop.query'' Nothing

  -- ocanren "prop" Prop.query Nothing
  -- ocanren "prop2" Prop.query2  Nothing
  -- ocanren "prop3" Prop.query3  Nothing
  -- ocanren "prop4" Prop.query4  Nothing
  -- ocanren "propSimple" Prop.query' Nothing

  -- ocanren "propCompl4" Prop.query2''' Nothing
  -- ocanren "plainEvaloDescends" Prop.plainQuery Nothing

  -- ocanren "appLengtho"  (appLengtho $ (call "appLengtho" [])) Nothing


  -- ocanren "sort" (sorto $ fresh ["x", "y"] (call "sorto" [V "x", V "y"])) Nothing
  -- ocanren "palindromo" (palindromo $ fresh ["xs"] (call "palindromo" [V "xs"])) Nothing

  -- ocanren "memApp" (memApp $ fresh ["h", "xs", "ys", "zs"] (call "memApp" [V "h", V "xs", V "ys", V "zs"])) Nothing

  -- ocanren "memAppY" (memAppY $ fresh ["h", "xs", "ys", "zs"] (call "memAppY" [V "h", V "xs", V "ys", V "zs"])) Nothing


  -- ocanren "smallBridge"     (game2 $ fresh ["a", "b"] (call "getAnswer'" [V "a", C "some" [V "b"]])) $ Just Bridge.env

  -- ocanren "desert"          Desert.query''' $ Just Desert.env

  -- ocanren "maxLengtho"      (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"])) Nothing

  -- ocanren "pathNat" Path.queryNat $ Just Path.env
  -- ocanren "pathPair" Path.queryPair $ Just Path.env
  -- ocanren "pathPair1" Path.queryPair1 $ Just Path.env
  --
  -- ocanren "pathElem" Path.queryElem $ Just Path.env
  -- ocanren "pathElem1" Path.queryElem1 $ Just Path.env

  -- ocanren "unify" Unify.query $ Just Unify.env
  -- ocanren "bigBridge"       (topLevelBigBridge $ fresh ["a", "b"] (call "tlBigBridge" [V "a", V "b"])) $ Just Bridge.env

  -- ocanren "bottles"         Bottles.query $ Just Bottles.env


--  ocanren "fAndS" Sample1.query $ Nothing

-- ocanren "revacco" (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])) Nothing


{------------------------------------
  Working examples
-------------------------------------}
  -- ocanren "appNil"          (doubleAppendo $ fresh ["y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"])) Nothing
  -- ocanren "appNilY"         (doubleAppendo $ fresh ["x", "z", "r"] (call "doubleAppendo" [V "x", nil, V "z", V "r"])) Nothing
  -- ocanren "appNilZ"         (doubleAppendo $ fresh ["x", "y", "r"] (call "doubleAppendo" [V "x", V "y", nil, V "r"])) Nothing
  -- ocanren "appNilR"         (doubleAppendo $ fresh ["x", "y", "z"] (call "doubleAppendo" [V "x", V "y", V "z", nil])) Nothing
  -- ocanren "double"          (doubleAppendo $ fresh ["x", "y", "r"] (call "doubleAppendo" $ map V ["x", "y", "x", "r"])) Nothing
  -- ocanren "revAcco"         (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])) Nothing
  -- ocanren "check5"          (check5 $ fresh ["x"] (call "check5" [V "x"])) Nothing
  -- ocanren "checkList5"      (checkLocanren "smallBridge"     (game2 $ fresh ["a", "b"] (call "getAnswer'" [V "a", C "some" [V "b"]])) $ Just Bridge.envist5 $ fresh ["x"] (call "checkList5" [V "x"])) Nothing
  -- ocanren "checkListOther5" (checkList5' $ fresh ["x"] (call "checkList5" [V "x"])) Nothing
  -- ocanren "inBotho"         (inBotho $ fresh ["x", "l"] (call "inBotho" [V "x", C "a" [] % nil, V "l" ])) Nothing
  -- ocanren "maxLengtho"      (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"])) Nothing
  -- ocanren "maxo"            (maxo $ fresh ["x", "m"] (call "maxo" [V "x", V "m"])) Nothing
{-------------------------------------}

{-
  ocanren "unify" Unify.query $ Just Unify.env
  ocanren "desert"          Desert.query'' $ Just Desert.env
  ocanren "desertSecond"    Desert.query' $ Just Desert.env
  -- ocanren "smallBridge"     (game2 $ fresh ["a", "b"] (call "getAnswer'" [V "a", C "some" [V "b"]])) $ Just Bridge.env
  -- ocanren "bigBridge"       (topLevelBigBridge $ fresh ["a", "b"] (call "tlBigBridge" [V "a", V "b"])) $ Just Bridge.env

  -- ocanren "bottles"         Bottles.query $ Just Bottles.env

  -- ocanren "revAcco" (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])) $ Nothing
  -- ocanren "bottles1"        Bottles.query' $ Just Bottles.env
  -}
    where
      ocanren filename goal env = do
        let (tree, logicGoal, names) = GC.topLevel goal Deterministic
        traceM ("\n\n NAMES \n\n" ++ show (vident <$> reverse names) )
        let path = printf "test/out/%s" filename
        exists <- doesDirectoryExist path
        if exists
        then removeDirectoryRecursive path
        else return ()
        let pathLocal = printf "%s/local" path
        createDirectoryIfMissing True path
        printTree (printf "%s/global.dot" path) tree
        createDirectoryIfMissing True pathLocal
        let nodes = GC.getNodes tree
        for_
          nodes
          (\(goal, tree) ->
            printTree (printf "%s/%s.dot" pathLocal (filter (/=' ') $ show goal)) tree
          )
        system (printf "dot -O -Tpdf %s/*.dot" path)
        system (printf "dot -O -Tpdf %s/*.dot" pathLocal)
        let prog = residualizationTopLevel tree
        writeFile (printf "%s/%s.before.pur" path filename) (show prog)
        traceM "Wait"
        let pur@(goal, xs, defs) = purification (prog, vident <$> reverse names)
        traceM ("LKSDJFLKJS: " ++ show xs)
        traceM "waht"
        traceM "goal"
        traceM (show $ length defs)
        traceM (show goal)
        let prog = Program defs goal
        writeFile (printf "%s/%s.pur" path filename) (show prog)
        traceM "Are"
        let ocamlCodeFileName = printf "%s/%s.ml" path filename
        traceM "you"
        OC.topLevel ocamlCodeFileName "topLevel" env pur
        traceM "kidding me?"
        print "doOcanrenize done"

-- doResidualization = do
--   purify "membero" $ membero $ fresh ["a"] (call "membero" [V "a", V "a" % nil])
--   -- purify "bottles.mk"         Bottles.query
--   -- purify "appNil.mk"          (doubleAppendo $ fresh ["y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"]))
--   --
--   -- purify "maxLengtho.mk"      (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"]))
--   -- purify "maxo.mk"            (maxo $ fresh ["x", "m"] (call "maxo" [V "x", V "m"]))
--   --
--   -- purify "revAcco.mk"         (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"]))
--   -- purify "double.mk"          (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" $ map V ["x", "y", "z", "r"]))
--   -- purify "appNil.mk"          (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"]))
--   -- purify "inBotho.mk"         (inBotho $ fresh ["x", "l"] (call "inBotho" [V "x", C "a" [] % nil, V "l" ]))
--   -- purify "smallBridge.mk"     (game2 $ fresh ["a", "b"] (call "getAnswer'" [V "a", C "some" [V "b"]]))
--   -- purify "bigBridge.mk"       (topLevelBigBridge $ fresh ["a", "b"] (call "tlBigBridge" [V "a", V "b"]))
--   -- purify "check5.mk"          (check5 $ fresh ["x"] (call "check5" [V "x"]))
--   -- purify "checkList5.mk"      (checkList5 $ fresh ["x"] (call "checkList5" [V "x"]))
--   -- purify "checkListOther5.mk" (checkList5' $ fresh ["x"] (call "checkList5" [V "x"]))
--   -- purify "desert.mk"          (Desert.query)
--   -- purify "desertSecond.mk"    (Desert.query')
--   -- --
--   -- -- -- purify "sudoku.mk"          (Sudoku4x4.query)
--   -- -- -- purify "sudokuValid.mk"     (Sudoku4x4.queryValid)
--   -- -- -- purify "sudokuInvalid.mk"   (Sudoku4x4.queryInvalid)
--   -- --
--   -- -- -- purify "logicInterpreter.mk" (logic_interpreter $ fresh ["subst", "fml", "res"] (call "check_subst" [V "subst", V "fml", V "res"]))
--   -- -- -- purify "commute.mk"         (appendo $ fresh ["a", "b", "c"] (call "appendo" [V "a", V "b", V "c"] &&& call "appendo" [V "b", V "a", V "c"]))
--   -- -- -- purify "listo.mk"           (appendo $ listo $ fresh ["a", "b", "c"] (call "listo" [V "a"] &&&
--   -- -- --                                                                       call "listo" [V "b"] &&&
--   -- -- --                                                                       call "listo" [V "c"] &&&
--   -- -- --                                                                       call "appendo" [V "a", V "b", V "c"] &&&
--   -- -- --                                                                       call "appendo" [V "b", V "a", V "c"]))

--     where
--       purify filename goal = do
--         let (tree, logicGoal, names) = GC.topLevel goal
--         let folder = "residualized"
--         createDirectoryIfMissing True folder
--         let result = residualizationTopLevel tree
--         writeFile (printf "%s/%s" folder filename) (printf "Before:\n%s\n\nAfter:\n%s" (show goal) (show result))


printGlobalStuff = do
  -- test "revacco" (revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"]))
  -- test "globalfAndS" Sample1.query

  -- test "globalBottles" Bottles.query

  -- test "bigBridge"       (topLevelBigBridge $ fresh ["a", "b"] (call "tlBigBridge" [V "a", V "b"]))

  -- test "globalSome"  (someAppendo $ fresh ["x", "y", "z"] (call "someAppendo" [V "x", V "y", V "z"]))

  test "maxLengtho"      (Program maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"]))

  -- test "global_path" Path.query1
  -- test "unify" Unify.query
  -- test "globalPath" Path.query1

  -- test "globalPath" Path.queryElem1


  -- test "globalDesert"          Desert.query

  -- test "unify_same" Unify.querySame
  -- test "unify" Unify.query
  -- test "getTerm" Unify.queryGet


  -- test "globalDouble"  (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "x", V "r"]))
  -- test "globalReverso"  (reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"]))

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
      showConj conj =
        intercalate "\n\n" $ map (\gss -> intercalate "\n" $ map show gss ) conj
      -- showConj set =
      --   let lst = Set.toList set in
      --   intercalate "\n\n" $ map show lst

      test fileName goal = do
        let tree =  GC.topLevel goal Deterministic
        writeFile (printf "%s.log" fileName) $ showConj $ globalTreeRetrieve $ fst3 tree
        printTree (printf "%s.dot" fileName) $ fst3 tree

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

checkEmbed4 =
  embed [add [V 509, inc 31 (V 459), inc 17 zero]]
        [add [V 517, inc 24 (V 1044), V 516]]
    where
      zero = C "o" []
      add args = Invoke "add" args
      inc 0 x = x
      inc n x = C "s" [inc (n-1) x]

checkEmbed5 =
  homeo (inc 31 (V 459) :: Term Int)
        (inc 24 (V 1044))
    where
      inc 0 x = x
      inc n x = C "s" [inc (n-1) x]

ch n m =
  homeo (inc n (V 459) :: Term Int)
        (inc m (V 1044))
    where
      inc 0 x = x
      inc n x = C "s" [inc (n-1) x]


-- statistics :: Int -> Int -> IO (Int, Int, (Double, Bool))
statistics x y = do
  let fileName = "stat.csv"
  fileExists <- doesFileExist fileName
  when fileExists (removeFile fileName)
  mapM (go fileName) [ (n, m) | n <- [0..x], m <- [0..y]]
  putStrLn "done"
    where
        go file (n, m) = do
          start <- getCPUTime
          let !r = homeo (inc n (V 0)) (inc m (V 1))
          end <- getCPUTime
          let t = (fromIntegral (end - start)) / (10^12)
          appendFile file (printf "%s, %s, %s, %s\n" (show n) (show m) (show t) (show r))
        inc 0 x = x
        inc n x = C "s" [inc (n-1) x]

checkEmbed''' =
  embed [getAnswer [V 455, st [false, false, true, false, true], some (V 459)], add [V 509, inc 31 (V 459), inc 17 zero]]
        [getAnswer [V 1039, st [false, false, true, false, true], some (V 1043)], add [V 1045, V 1043, V 1044], add [V 517, inc 24 (V 1044), V 516], add [V 1100, inc 62 (V 516), inc 17 zero]]
    where
      zero = C "o" []
      false = C "false" []
      true = C "true" []
      some x = C "some" [x]
      st x = C "state" x
      add args = Invoke "add" args
      getAnswer args = Invoke "getAnswer'" args
      inc 0 x = x
      inc n x = C "s" [inc (n-1) x]
--
-- in before checking
-- [getAnswer' v.455 (C st [false, false, true, false, true]) (C some [v.459]),add v.509 ((31 + v.459)) 17]
-- and
-- [getAnswer' v.1039 (C st [false, false, true, false, true]) (C some [v.1043]),add v.1045 v.1043 v.1044,add v.517 ((24 + v.1044)) v.516,add v.1100 ((62 + v.516)) 17]
