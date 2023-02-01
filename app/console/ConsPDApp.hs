module ConsPDApp where

import           Program
import           Syntax
import           System.FilePath    (takeBaseName)
import qualified Transformer.ConsPD

runWithParser :: (String -> IO (Either String (Program G X))) -> FilePath -> FilePath -> IO ()
runWithParser parser outDir inputFile = do
  res <- parser inputFile
  case res of
    Left err ->
      putStrLn err
    Right program ->
      Transformer.ConsPD.runConsPD' outDir (takeBaseName inputFile) program
