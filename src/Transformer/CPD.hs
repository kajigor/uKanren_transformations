{-# LANGUAGE BangPatterns #-}

module Transformer.CPD where

import           Control.Monad
import qualified CPD.GlobalControl        as GC
import qualified CPD.LocalControl         as LC
import           CPD.Residualization
import           Data.List
import           Data.Maybe
import qualified OCanrenize               as OC
import           Prelude                  hiding (succ)
import           Printer.Dot
import Printer.GlobalTree ()
import Printer.SldTree ()
import qualified Program.Bottles
import           Program.Programs
import           Program.Prop
import           Program.Unify
import           Purification
import           Residualization
import           Syntax
import           System.FilePath          ((</>), (<.>))
import           System.Process           (system)
import           Text.Printf
import qualified Transformer.MkToProlog
import qualified GHC.IO.Exception
import Util.File ( createDirRemoveExisting, shortenFileName )

data TransformResult = Result { original :: [Def]
                              , globalTree :: GC.GlobalTree
                              , localTrees :: [([G S], LC.SldTree)]
                              , beforePur :: Program
                              , purified :: (G X, [X], [Def])
                              }

runTransformation :: Program -> LC.Heuristic -> TransformResult
runTransformation goal@(Program original _) heuristic =
  let (globalTree, logicGoal, names) = GC.topLevel goal heuristic in
  let localTrees = GC.getNodes globalTree in
  let beforePur = residualizationTopLevel globalTree in
  let purified = purification (beforePur, vident <$> reverse names) in
  Result original globalTree localTrees beforePur purified

generatePdf :: FilePath -> IO GHC.IO.Exception.ExitCode
generatePdf dir =
  system (printf "dot -O -Tpdf %s/*.dot" dir)

renderLocalTree :: FilePath -> [G S] -> LC.SldTree -> IO ()
renderLocalTree localDir goal =
    printTree (localDir </> shortenFileName (show goal) <.> "dot")

transform' :: FilePath -> FilePath -> Program -> Maybe String -> LC.Heuristic -> IO ()
transform' outDir filename goal@(Program definitions _) env heuristic = do
    let path = outDir </> filename
    let localDir = path </> "local"
    let cpdFile = path </> "cpd"
    mapM_ createDirRemoveExisting [path, localDir]

    let result = runTransformation goal heuristic
    Transformer.MkToProlog.transform (path </> "original.pl") definitions
    printTree (path </> "global.dot") (globalTree result)
    mapM_ (uncurry (renderLocalTree localDir)) (localTrees result)
    writeFile (cpdFile <.> "before.pur") (show $ beforePur result)
    let pur@(goal,xs,defs) = purified result
    Transformer.MkToProlog.transform (cpdFile <.> "pl") defs
    let purified = Program defs goal
    writeFile (cpdFile <.> "pur") (show purified)
    let ocamlCodeFileName = cpdFile <.> "ml"
    OC.topLevel ocamlCodeFileName "topLevel" env pur

    mapM_ generatePdf [path, localDir]

transform :: FilePath -> Program -> Maybe String -> LC.Heuristic -> IO ()
transform = transform' "test/out/cpd"

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
      ocanren = transform
