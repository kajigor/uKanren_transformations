module NormalizeApp where

import Parser (parseWholeProgram)
import Syntax
import Text.Printf (printf)
import Util.File (checkIfFileExists, failIfNotExist, prologExt)
import qualified Transformer.MkToProlog
import NormalizedSyntax

run fileName = do
  failIfNotExist fileName
  program <- readFile fileName
  case parseWholeProgram program of
    Left err ->
      putStrLn err
    Right p -> do
      putStrLn "Initial program\n"
      putStrLn program
      putStrLn "\nTransformed\n"
      print $ normalizeProg p