module ConsPDApp where

import           Program
import           Syntax
import           System.FilePath    (takeBaseName)
import qualified Transformer.ConsPD
import qualified CPD.LocalControl as LC

runWithParser :: (String -> IO (Either String (Program G X))) -> FilePath -> Maybe [Int] -> FilePath -> LC.Heuristic -> IO ()
runWithParser parser outDir ground inputFile heu = do
  res <- parser inputFile
  case res of
    Left err ->
      putStrLn err
    Right program ->
      Transformer.ConsPD.runConsPD' outDir ground (takeBaseName inputFile) program
