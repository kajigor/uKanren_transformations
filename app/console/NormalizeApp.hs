module NormalizeApp where

import Syntax
import Text.Printf (printf)
import Util.File (checkIfFileExists, failIfNotExist, prologExt)
import qualified Transformer.MkToProlog
import NormalizedSyntax

runWithParser parser fileName = do
  res <- parser fileName
  case res of
    Left err ->
      putStrLn err
    Right p -> do
      putStrLn "Initial program\n"
      putStrLn (show p)
      putStrLn "\nTransformed\n"
      let normalized = normalizeProg p
      print normalized
      putStrLn "\nAnd back\n"
      print $ toSyntax normalized