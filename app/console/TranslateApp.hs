module TranslateApp where

import Syntax
import Program
import Mode.Toplevel
import FunConversion.Trans (transProg)
import FunConversion.Syntax (embedProgSafe)
import Language.Haskell.TH (pprint)
runWithParser :: (t -> IO (Either String (Program G String))) -> t -> String -> [Int] -> IO ()
runWithParser parser inputFile relName inputs = do
  program <- parser inputFile
  case program of
    Left err ->
      putStrLn err
    Right p ->
      case transProg relName inputs p of
        Right pr ->
          case embedProgSafe relName pr of
            Right hs ->
                putStrLn $ pprint hs
            Left err ->
                putStrLn $ "Intermediate->Haskell translation failed:\n" ++ show err
        Left err ->
          putStrLn $ "miniKanren->Intermediate translation failed:\n" ++ show err