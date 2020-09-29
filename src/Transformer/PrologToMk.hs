module Transformer.PrologToMk where

import qualified OCanrenize      as OC
import           System.FilePath (replaceExtension, (</>))
import           System.IO
import           Text.Printf
import           Util.ToProlog

transform dirName file = do
  let fileName = dirName </> file
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let ocamlCodeFileName = replaceExtension fileName "ml"
  let g = prologToG contents
  let env = Nothing
  OC.topLevel ocamlCodeFileName "topLevel" env g
