module Transformer.SymbolicExecution where

import           Control.Monad     (when)
import           Printer.Dot
import           Printer.SymTree   ()
import           SymbolicExecution (topLevel)
import           System.Directory
import           System.FilePath   ((</>))
import           System.Process    (system)
import           Text.Printf
import           Util.System       (graphsToPdf)
import           Util.File         (removeDirIfExists)

transform n filename goal = do
  let tree = topLevel n goal
  let path = "test/out/sym" </> filename
  removeDirIfExists path 
  createDirectoryIfMissing True path
  printTree (path </> "tree.dot") tree
  graphsToPdf path
  return ()
