module ParseApp where

import Parser (parseDefs)
import Text.Printf (printf)
import Util.File (checkIfFileExists, prologExt)
import qualified Transformer.MkToProlog

run dir file = do
  fileName <- checkIfFileExists dir file
  program <- readFile fileName
  case parseDefs program of
    Left err ->
      putStrLn err
    Right defs -> do
      let prologFile = prologExt fileName
      Transformer.MkToProlog.transform prologFile defs
      prolog <- readFile prologFile
      putStrLn $ printf "\nProlog'ed:\n\n%s\n" prolog