module Transformer.PD where

import qualified PD.PartialDeduction as PD
import           PD.Residualization
import           Printer.Dot         (printTree)
import           Printer.PDTree      ()
import           System.FilePath     ((</>), (<.>), takeBaseName)
import           System.Process      (system)
import           Util.File           (createDirRemoveExisting)
import           Util.System         (graphsToPdf)
import Util.Miscellaneous (trd3)
import Residualization (vident)
import Purification
import qualified OCanrenize as OC 
import qualified Transformer.MkToProlog

import Syntax
import Def
import Program 
import Control.Monad 
import Data.Maybe

data TransformResult = Result
  { original :: [Def G X],
    tree :: PD.PDTree,
    simplifiedTree :: PD.PDTree,
    names :: [X],
    beforePurification :: Maybe (G X, [X], [Def G X]),
    purified :: Maybe (G X, [X], [Def G X])
  }

runTransformation :: Program G X -> TransformResult
runTransformation goal@(Program original _) =
  let transformed@(tree, logicGoal, names) = PD.topLevel goal in
  let namesX = vident <$> reverse names in
  let simplifiedTree = PD.simplify tree in
  let residualized = residualize transformed in
  let beforePur = justTakeOutLetsProgram residualized namesX in
  let purified = purification (residualized, namesX) in
  Result original tree simplifiedTree namesX (Just beforePur) (Just purified)

toOcanren :: FilePath -> Program G X -> [String] -> IO ()
toOcanren fileName (Program defs goal) names =
  OC.topLevel fileName "topLevel" Nothing (goal, names, defs)

transform inputFile outDir program@(Program definitions _) = do
  let env = Nothing  
  let pdDir = "pd"
  let filename = takeBaseName inputFile 
  let path = outDir </> takeBaseName inputFile </> pdDir

  let result = runTransformation program

  createDirRemoveExisting path
  toOcanren (path </> "original.ml") program (names result)
  Transformer.MkToProlog.transform (path </> "original.pl") definitions
  printTree (path </> "tree.dot") (tree result)
  printTree (path </> "tree.simplified.dot") (simplifiedTree result)
  printTree (path </> "tree.restricted.dot") (PD.restrictSubsts $ simplifiedTree result)

  graphsToPdf path

  guard (isJust $ beforePurification result)
  let before = fromJust $ beforePurification result
  let ocamlCodeFileName = path </> filename <.> "before.ml"
  OC.topLevel ocamlCodeFileName "topLevel" env before
  Transformer.MkToProlog.transform (path </> "before.pl") (trd3 before)

  guard (isJust $ purified result )
  let pur@(goal', _, defs') = fromJust $ purified result
  let prog = Program defs' goal'
  Transformer.MkToProlog.transform (path </> filename <.> "pl") defs'
  let ocamlCodeFileName = path </> filename <.> "ml"
  OC.topLevel ocamlCodeFileName "topLevel" env pur

  return ()
