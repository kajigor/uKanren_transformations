module TranslateApp where

import Syntax
import Program
import FunConversion.Trans (transProg)
import FunConversion.Syntax (embedProgSafe)
import Language.Haskell.TH (pprint)
runWithParser :: (t -> IO (Either String (Program G String))) -> t -> String -> [Int] -> IO ()
runWithParser parser inputFile relName inputs = do
  program <- parser inputFile
  let pr = program >>= transProg relName inputs >>= embedProgSafe relName
  putStrLn $ case pr of
    Left err -> "Error: " ++ err
    Right hs -> pprint hs