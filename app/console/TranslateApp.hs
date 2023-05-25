module TranslateApp where

import Syntax
import qualified FunConversion.DetCheck as Det
import Program
import FunConversion.Trans (transProg)
import FunConversion.Syntax (embedProgSafe)
import Language.Haskell.TH (pprint)
import FunConversion.OCamlPretty (prettyString)

runWithParser :: (t -> IO (Either String (Program G String))) -> t -> String -> [Int] -> IO ()
runWithParser parser inputFile relName inputs = do
  program <- parser inputFile
  let translatedProgram = program >>= transProg relName inputs
  case translatedProgram of
    Left err -> putStrLn $ "Error: " ++ err
    Right hs -> do
      let ocamlPr = prettyString hs
      putStrLn ocamlPr
      putStrLn "-------------------------"
      -- let pr = embedProgSafe relName hs
      let pr = Det.embedProgDet relName hs
      putStrLn $ case pr of
        Left err -> "Error: " ++ err
        Right hs -> pprint hs