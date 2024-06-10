module CPDApp where

import           CPD.LocalControl
import           Program
import           Syntax
import           System.FilePath  (takeBaseName)
import qualified Transformer.CPD

runWithParser :: (String -> IO (Either String (Program G X))) -> FilePath -> Maybe [Int] -> FilePath -> CPD.LocalControl.Heuristic -> IO ()
runWithParser parser outDir ground inputFile heu = do
  res <- parser inputFile
  case res of
    Left err ->
      putStrLn err
    Right program -> do 
      res <- Transformer.CPD.transform' outDir (takeBaseName inputFile) program Nothing heu
      return ()
