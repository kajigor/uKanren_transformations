module ParseApp where

import Syntax
import Text.Printf (printf)
import Util.File (checkIfFileExists, failIfNotExist, prologExt)
import qualified Transformer.MkToProlog

run parser fileName = do
  failIfNotExist fileName
  program <- readFile fileName
  case parser program of
    Left err ->
      putStrLn err
    Right (Program defs goal) -> do
      let prologFile = prologExt fileName
      Transformer.MkToProlog.transform prologFile defs
      prolog <- readFile prologFile
      putStrLn $ printf "\nProlog'ed:\n\n%s\n" prolog
      putStrLn $ printf "\nGoal:\n\n%s\n" (show goal)