module ConsPDApp where

import           Program
import           Syntax
import           System.FilePath    (takeBaseName)
import qualified Transformer.ConsPD
import Debug.Trace

runWithParser :: (String -> IO (Either String (Program G X))) -> FilePath -> Maybe [Int] -> FilePath -> IO ()
runWithParser parser outDir ground inputFile = do
  res <- parser inputFile
  case res of
    Left err ->
      putStrLn err
    Right program ->
      Transformer.ConsPD.runConsPD' outDir ground (takeBaseName inputFile) (trace "runConsPD'" program)
