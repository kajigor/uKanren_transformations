module TranslateApp where

import Syntax
import qualified FunConversion.DetCheck as Det
import Program
import FunConversion.Trans (transProg)
import FunConversion.Syntax (embedProgSafe)
import Language.Haskell.TH (pprint)
import FunConversion.OCamlPretty (prettyString)
import           System.FilePath        ((<.>), (</>), takeBaseName)
import           Util.File              (createDirRemoveExisting)
import System.Directory (copyFile)

runWithParser :: (FilePath -> IO (Either String (Program G String))) -> FilePath -> FilePath -> String -> [Int] -> IO ()
runWithParser parser inputFile outDir relName inputs = do
  createDirRemoveExisting outDir
  let baseName = takeBaseName inputFile
  let outFile = outDir </> baseName
  let ocamlFile = outFile <.> "ml"
  let haskellFile = outFile <.> "hs"
  copyFile inputFile (outFile <.> "mk")

  program <- parser inputFile
  let translatedProgram = program >>= transProg relName inputs
  case translatedProgram of
    Left err -> putStrLn $ "Error in translating: " ++ err
    Right hs -> do
      let ocamlPr = prettyString hs
      writeFile ocamlFile ocamlPr

      let pr = embedProgSafe relName hs
      case pr of
        Left err -> putStrLn $ "Template Haskell error: " ++ err
        Right hs ->
          writeFile haskellFile (pprint hs)