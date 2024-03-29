module Transformer.PrologToMk where

import qualified OCanrenize    as OC
import           System.IO
import           Util.File     (ocamlExt)
import           Util.ToProlog

transform :: FilePath -> IO ()
transform fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let ocamlCodeFileName = ocamlExt fileName
  let g = prologToG contents
  let env = Nothing
  OC.topLevel ocamlCodeFileName "topLevel" env g
