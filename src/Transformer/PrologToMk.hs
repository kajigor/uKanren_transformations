module Transformer.PrologToMk where

import qualified OCanrenize      as OC
import           System.FilePath ((<.>))
import           System.IO
import           Text.Printf
import           Util.ToProlog

transform fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let ocamlCodeFileName = fileName <.> "ml"
  let g = prologToG contents
  let env = Nothing
  OC.topLevel ocamlCodeFileName "topLevel" env g
