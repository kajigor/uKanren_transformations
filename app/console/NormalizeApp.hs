module NormalizeApp where

import NormalizedSyntax ( normalizeProg, toSyntax )
import qualified Syntax
import Program

runWithParser :: (t -> IO (Either String (Program Syntax.G Syntax.X))) -> t -> IO ()
runWithParser parser fileName = do
  res <- parser fileName
  case res of
    Left err ->
      putStrLn err
    Right p -> do
      putStrLn "Initial program\n"
      print p
      putStrLn "\nTransformed\n"
      let normalized = normalizeProg p
      print normalized
      putStrLn "\nAnd back\n"
      print $ toSyntax normalized