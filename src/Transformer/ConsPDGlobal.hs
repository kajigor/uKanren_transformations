{-# LANGUAGE BangPatterns #-}

module Transformer.ConsPDGlobal where

import           Control.Monad
import qualified ConsPD.GlobalControl      as GC
import qualified ConsPD.LocalControl       as LC 
import           ConsPD.GlobalResidualization
import           Data.List
import           Data.Maybe
import           Def
import qualified GHC.IO.Exception
import qualified OCanrenize              as OC
import           Prelude                 hiding (succ)
import           Printer.Dot
import           Printer.GlobalTreeCons  ()
import           Printer.SldTreeCons     ()
import           Printer.PrettyMkPrinter (prettyMk)
import           Program
import           Purification
import           Residualization
import           Syntax
import           System.FilePath        ((<.>), (</>), takeBaseName)
import           System.Process         (system)
import           Text.Printf
import qualified Transformer.MkToProlog
import           Util.File              (createDirRemoveExisting, shortenFileName)
import           Util.System            (graphsToPdf)
import           Debug.Trace (traceShow)
import Util.System (graphsToPdf)

data TransformResult = Result { original   :: [Def G X]
                              , globalTree :: GC.GlobalTree
                              , localTrees :: [([G S], LC.SldTree)]
                              , beforePur  :: Program G X
                              , purified   :: (G X, [X], [Def G X])
                              }

runTransformation :: Program G X -> LC.Heuristic -> TransformResult
runTransformation goal@(Program original _) heuristic =
  let (globalTree, logicGoal, names) = GC.topLevel goal heuristic in
  let localTrees = GC.getNodes globalTree in
  let beforePur = residualizationTopLevel globalTree in
  let purified = purification (beforePur, vident <$> reverse names) in
  Result original globalTree localTrees beforePur purified

renderLocalTree :: FilePath -> [G S] -> LC.SldTree -> IO ()
renderLocalTree localDir goal =
    printTree (localDir </> shortenFileName (show goal) <.> "dot")

transform' :: FilePath -> FilePath -> Program G X -> Maybe String -> LC.Heuristic -> IO (Program G X)
transform' outDir filename goal@(Program definitions _) env heuristic = do
    let cpdDir = "conspdGlobal"
    let path = outDir </> takeBaseName filename </> cpdDir </> show heuristic
    let localDir = path </> "local"
    let cpdFile = path </> "conspdGlobal"
    mapM_ createDirRemoveExisting [path, localDir]

    let result = runTransformation goal heuristic
    Transformer.MkToProlog.transform (path </> "original.pl") definitions
    printTree (path </> "global.dot") (globalTree result)
    mapM_ (uncurry (renderLocalTree localDir)) (localTrees result)
    writeFile (cpdFile <.> "before.pur") (prettyMk $ beforePur result)
    let pur@(goal,xs,defs) = purified result
    Transformer.MkToProlog.transform (cpdFile <.> "pl") defs
    let purified = Program defs goal
    writeFile (cpdFile <.> "pur") (prettyMk purified)
    let ocamlCodeFileName = cpdFile <.> "ml"
    OC.topLevel ocamlCodeFileName "topLevel" env pur

    mapM_ graphsToPdf [path, localDir]
    return purified

transform :: FilePath -> Program G X -> Maybe String -> LC.Heuristic -> IO ()
transform filePath pr str heu = do  
  res <- transform' "test/out/conspdGlobal" filePath pr str heu
  return ()
