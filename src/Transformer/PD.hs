module Transformer.PD where

import           Control.Monad    (when)
import qualified PartialDeduction as PD
import           Printer.Dot
import           Printer.PDTree   ()
import           System.Directory
import           System.FilePath  ((</>))
import           System.Process   (system)
import           Text.Printf

transform filename goal = do
  let (tree, logicGoal, names) = PD.topLevel goal
  let path = "test/out/pd" </> filename
  exists <- doesDirectoryExist path
  when exists (removeDirectoryRecursive path)
  createDirectoryIfMissing True path
  printTree (path </> "tree.dot") tree
  system (printf "dot -O -Tpdf %s/*.dot" path)
  return ()
