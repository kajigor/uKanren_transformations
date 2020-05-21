{-# LANGUAGE BangPatterns #-}

module Main (main, tests) where

import           Control.Monad
import qualified CPD.GlobalControl        as GC
import           CPD.LocalControl
import           CPD.Residualization
import           Data.Foldable            (for_)
import           Data.List
import           Data.Maybe
import qualified Data.Set                 as Set
import           Debug.Trace
import qualified Eval                     as E
import qualified OCanrenize               as OC
import           Prelude                  hiding (succ)
import           Printer.Dot
import           Printer.GlobalTree
import           Printer.SldTree
import           Program.Bool
import qualified Program.Bottles
import           Program.Bridge
import qualified Program.Desert
import           Program.List
import           Program.LogicInterpreter
import           Program.Num
import           Program.Path             hiding (elem)
import           Program.Programs
import           Program.Prop
import           Program.Sample1
import           Program.Sort
import           Program.SpecialProp
import qualified Program.Sudoku4x4
import           Program.Unify
import           Purification
import           Residualize
import           Syntax
import           System.Directory
import           Text.Printf
import           Unfold
import           Util.ConjRetriever
import           Util.Miscellaneous
import           Embed
import           System.CPUTime
import           System.IO
import           System.Process           (system)
import           System.TimeIt


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
      printTree (printf "%s/local.dot" path) $ topLevel goal

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
        let (tree, logicGoal, names) = GC.topLevel goal
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
        let tree =  GC.topLevel goal
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
        app00D = Descend app00 []
        app01D = Descend app01 []
        app10 = app xs' ys t'
        app11 = app (cons h t') zs r
        app10D = Descend app10 [app00]
        app11D = Descend app11 []
        app20 = app10
        app21 = app t' zs r'
        app20D = Descend app20 [app00]
        app21D = Descend app21 [app11]
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
        max0D = Descend max0 []
        len0D = Descend len0 []
        max1 = max' t n m
        len1 = len (cons h t) l
        max1D = Descend max1 [max0]
        len1D = Descend len1 []
        max2 = max' t n m
        len2 = len t k
        max2D = Descend max2 [max0]
        len2D = Descend len2 [len1]

-- testTakingOutLets = do
--   assert "taking out lets 0" (uni0, [], []) (justTakeOutLets (uni0, []))
--   assert "taking out lets 1" (uni1, [], [(defName, args0', uni0')]) (justTakeOutLets (flatlet, []))
--   assert "taking out lets 2" (conj, [], [(defName, args0'', uni0''), (fedName, args2', disj')]) (justTakeOutLets (doublet, []))
--   where
--     x = V "x"
--     y = V "y"
--     z = V "z"
--     y0 = V "y0"
--     y1 = V "y1"
--     y2 = V "y2"
--     y3 = V "y3"
--     y4 = V "y4"
--     uni0 = x === y
--     uni1 = y === z
--     uni2 = x === z
--     conj = uni0 &&& uni1
--     disj = conj ||| uni2
--     uni0' = y0 === y1
--     uni0'' = y3 === y4
--     uni1' = y1 === y2
--     uni2' = y0 === y2
--     conj' = uni0' &&& uni1'
--     disj' = conj' ||| uni2'
--     defName = "def"
--     fedName = "fed"
--     args0 = []
--     args1 = ["x"]
--     args0'  = ["y0", "y1"]
--     args0'' = ["y3", "y4"]
--     args1' = ["y2"]
--     args2' = ["y1", "y2", "y0"]
--     flatlet = Let (Def defName args0 uni0) uni1
--     doublet = Let (Def defName args0 uni0) (Let (Def fedName args1 disj) conj)

testPopingOutFreshes = do
  assert "popping freshes up 0" (callF x' y') (fst3 $ E.preEval E.env0 (fresh ["x", "y"] goal))
  assert "popping freshes up 1" (fLet (callF x' y')) (fst3 $ E.preEval E.env0 (fresh ["x", "y"] $ fLet goal))
  assert "popping freshes up 2" (body', reverse [0..4]) ((\(x, _, y) -> (x, y)) $ E.preEval E.env0 (fresh ["m", "n"] body))
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
    fLet = Let (Def "f" ["m", "n"] body)
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

testUnifySubsts = do
  putStrLn "testing unifySubsts"
  assert "unify 0" (Just []) (E.unifySubsts [] [])
  assert "unify 1" Nothing (E.unifySubsts [(1, V 2)] [])
  assert "unify 2" Nothing (E.unifySubsts [(1, C "d" [])] [(1, C "c" [])])

testLocalControl = do
  manyAssertCustom "local control" isVariant [[app x y z], [app x y t, app t z r]] (leaves $ topLevel (Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])))
  manyAssertCustom "local control" isVariant [[app x y z]] (leaves $ topLevel (Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"])))
  assertCustom "local control" (\x y -> length x == length y) [] (leaves $ topLevel (Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [C "Stupid" [], V "y", V "z", V "r"])))
  manyAssertCustom "local control" isVariant [ [ maxo1 x zero z, lengtho x y ]
                                             , [ maxo1 (x % y) (succ z) r, lengtho y t ]
                                             , [ leo z u trueo, maxo1 (x % y) (succ (succ u)) r, lengtho y t ]
                                             , [ maxo1 (x % y) (succ (succ r)) t, lengtho y z]
                                             , [ gto z r trueo, maxo1 (x % y) (succ (succ z)) t, lengtho y u ]
                                             ]
                                             (leaves $ topLevel (Program maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"])))
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
  assert "minimally general 0" [f x x]          (minimallyGeneral' [[f x y], [f x x]])
  assert "minimally general 1" [f x x]          (minimallyGeneral' [[f x x], [f x y]])
  assert "minimally general 2" [g x x y]        (minimallyGeneral' [[g x x y], [g x y y], [g x y x], [g x y z]])
  assert "minimally general 3" [g x y y]        (minimallyGeneral' [[g x y z], [g x y y], [g x y x], [g x x y]])
  assert "minimally general 4" [f x y, g x y z] (minimallyGeneral' [[f x y, g x y z], [f x z], [f x x], [f x x, g x y z]]) -- y and x are linked and the selected one is the first
  assert "minimally general 5" [f x x]          (minimallyGeneral' [[f x z], [f x x], [f x x, g y y z], [f x y, g z t u]])
  assert "minimally general 6" [f x z, g x y z] (minimallyGeneral' [[f x z, g x y z], [f x x, g y y z], [f x y, g z t u]])
  assert "minimally general 7" [f x x, g y y z] (minimallyGeneral' [[f x x, g y y z], [f x y, g z t u], [f x z, g x y z]])
  assert "minimally general 8" [p u y, q y z]   (minimallyGeneral' [[p u y, q y z], [p x u, q y z]])
  assert "minimally general 9" [p u y, q y z]   (minimallyGeneral' [[p x u, q y z], [p u y, q y z]])
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

    minimallyGeneral' = fst . minimallyGeneral . map (\x -> (x, []))

testSplit = do -- TODO more tests
  assertCustom "split 0" checkVariant ([f x x], [g x]) (fst3 $ split [2..] [f x x] [f x x, g x] )
  assertCustom "split 1" checkVariant ([f x x], [g x]) (fst3 $ split [2..] [f x x] [g x, f x x] )
  assertCustom "split 2" checkVariant ([f x z], [g x]) (fst3 $ split [2..] [f x y] [g x, f x x] )
  assertCustom "split 3" checkVariant ([maxo1 (v150 % v153) (s v152) v1, lengtho v153 v154], [leo v121 v122])
                                      (fst3 $
                                       split [150..]
                                             [maxo1 (v51 % v52) (s v33) v1, lengtho v52 v53]
                                             [leo v121 v122, maxo1 (v126 % v127) (s (s (s (s (s v122))))) v1, lengtho v127 v128])
  assertCustom "split 4" checkVariant ([maxo1 v50 (s (s v51)) v1], [])
                                      (fst3 $
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
  assert "abstract" [goal] (map fst $ fst $ GC.abstract (Descend goal []  ) goal [11..])
  where
    goal = [maxo1 v3 zero v1]
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    v3 = V 3
    v1 = V 1
    zero = C "O" []

sa1 = Invoke "a" [V 1, C "c" [V 4, C "c" [V 5, C "n" []]] ,
                       C "c" [V 4, C "c" [V 6, C "n" []]]]

dsa1 = [Invoke "a" [V 5, V 1, V 6], sa1]

sa2 = Invoke "a" [V 1, C "c" [V 4, C "c" [V 7, C "c" [V 8, C "n" []]]] ,
                       C "c" [V 4, C "c" [V 7, C "c" [V 9, C "n" []]]]]

dsa2 = [Invoke "a" [V 8, V 1, V 9 ], sa2]


littleInstTest = do
  assert "little instance check" True (isInst
                                              ( ca (pair v73 zero) v45 (pair v73 zero) v2 true )
                                              ( ca (pair v28 zero) v4  (pair v28 v43 ) v2 true )
                                              )
  assert "little instance check" True (isInst
                                              ( ca (pair v28 zero) v4  (pair v28 v43 ) v2 true )
                                              ( ca (pair v73 zero) v45 (pair v73 zero) v2 true )
                                              )
    where
      ca x y z m n = Invoke "checkAnswer'" [x, y, z, m, n]
      pair x y = C "pair" [x, y]
      true = C "true" []
      zero = C "zero" []
      v2 = V 2
      v4 = V 4
      v28 = V 28
      v43 = V 43
      v45 = V 45
      v73 = V 73
                                        -- [checkAnswer' (C pair [v.73, 0]) v.45 (C pair [v.73, 0]) v.2 true]
                                        --
                                        -- [checkAnswer' (C pair [v.28, 0]) v.4 (C pair [v.28, v.43]) v.2 true]
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

testGround = do
  test ([] :: [G X]) True
  test x False
  test nil True
  test (cons x nil) False
  test (cons two nil) True
  test (eq true true true) True
  test (gt zero five false) True
  test [eq true true true] True
  test [checkPerson (st true true true true true) x true] False
  test (maxim zero five x) False
  test [ga x (st true true true true true) (some x), times b x, times d x, maxim x x x, add x x x, add x x five] False
    where
      test x expected = assert "Ground" expected (isGround x)
      x = V "x"
      y = V "y"
      b = C "b" []
      d = C "d" []
      true = C "true" []
      false = C "false" []
      nil = C "Nil" []
      cons h t = C "Cons" [h, t]
      some x = C "Some" [x]
      z = C "O" []
      s x = C "S" [x]
      two = s $ s z
      five = s . s . s . s . s $ z
      st x y z m n = C "st" [x, y, z, m, n]
      eq x y r = Invoke "eq" [x, y, r]
      gt x y r = Invoke "gt" [x, y, r]
      checkPerson x y r = Invoke "checkPerson" [x, y, r]
      maxim x y r = Invoke "max" [x, y, r]
      ga x y r = Invoke "getAnswer'" [x, y, r]
      times x y = Invoke "times" [x, y]
      add x y r = Invoke "add" [x, y, r]
