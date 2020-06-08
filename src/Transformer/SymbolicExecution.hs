module Transformer.SymbolicExecution where

import           Printer.Dot
import           Printer.SymTree   ()
import           SymbolicExecution (topLevel)
import           System.Directory
import           System.Process    (system)
import           Text.Printf

transform n filename goal = do
  let tree = (topLevel n) goal
  let path = printf "test/out/sym/%s" filename
  exists <- doesDirectoryExist path
  if exists
  then removeDirectoryRecursive path
  else return ()
  createDirectoryIfMissing True path
  printTree (printf "%s/tree.dot" path) tree
  system (printf "dot -O -Tpdf %s/*.dot" path)
  return ()
