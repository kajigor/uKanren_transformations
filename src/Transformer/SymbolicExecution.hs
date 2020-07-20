module Transformer.SymbolicExecution where

import           Printer.Dot
import           Printer.SymTree   ()
import           SymbolicExecution (topLevel)
import           System.Directory
import           System.FilePath   ((</>))
import           System.Process    (system)
import           Text.Printf

transform n filename goal = do
  let tree = (topLevel n) goal
  let path = "test/out/sym" </> filename
  exists <- doesDirectoryExist path
  if exists
  then removeDirectoryRecursive path
  else return ()
  createDirectoryIfMissing True path
  printTree (path </> "tree.dot") tree
  system (printf "dot -O -Tpdf %s/*.dot" path)
  return ()
