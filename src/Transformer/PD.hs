module Transformer.PD where

import qualified PartialDeduction as PD
import           Printer.Dot
import           Printer.PDTree   ()
import           System.Directory
import           System.Process   (system)
import           Text.Printf

transform filename goal = do
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
