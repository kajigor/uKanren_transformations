module ModeApp where

import Syntax
import Program
import Mode.Toplevel

runWithParser :: (t -> IO (Either String (Program G String))) -> t -> String -> [Int] -> IO ()
runWithParser parser inputFile relName inputs = do
  program <- parser inputFile
  case program of
    Left err ->
      putStrLn err
    Right p ->
      print $ topLevelWithDefaultCall p relName inputs
