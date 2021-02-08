module Transformer.PrologToMk where

import qualified OCanrenize      as OC
import           System.FilePath ((</>))
import           System.IO
import           Util.ToProlog
import Util.File ( ocamlExt )

transform :: FilePath -> FilePath -> IO ()
transform dirName file = do
  let fileName = dirName </> file
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let ocamlCodeFileName = ocamlExt fileName
  let g = prologToG contents
  let env = Nothing
  OC.topLevel ocamlCodeFileName "topLevel" env g