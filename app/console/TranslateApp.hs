{-# LANGUAGE TupleSections #-}

module TranslateApp where

import Syntax
import qualified FunConversion.DetCheck as Det
import Program
import FunConversion.Trans (transProg, transMultiMode)
import FunConversion.Syntax (embedProgSafe)
import Language.Haskell.TH (pprint)
import FunConversion.OCamlPretty (prettyString)
import           System.FilePath        ((<.>), (</>), takeBaseName)
import           Util.File              (createDirRemoveExisting)
import System.Directory (copyFile)
import Data.List (subsequences)
import Util.String

runAllDirections :: (FilePath -> IO (Either String (Program G String))) -> FilePath -> FilePath -> String -> IO ()
runAllDirections parser inputFile outDir relName = do
  createDirRemoveExisting outDir
  let baseName = takeBaseName inputFile
  let uBaseName = toUpper baseName
  let outFile = outDir </> baseName
  let uOutFile = outDir </> uBaseName
  let ocamlFile = uOutFile <.> "ml"
  let haskellFile = uOutFile <.> "hs"
  copyFile inputFile (outFile <.> "mk")

  program <- parser inputFile
  let inputs = subsequences [0..2]
  let translatedProgram = program >>= \p -> transMultiMode (getDefs p) (map (relName,) inputs)
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

runWithParser :: (FilePath -> IO (Either String (Program G String))) -> FilePath -> FilePath -> String -> [Int] -> IO ()
runWithParser parser inputFile outDir relName inputs = do
  createDirRemoveExisting outDir
  let baseName = takeBaseName inputFile
  let uBaseName = toUpper baseName
  let outFile = outDir </> baseName
  let uOutFile = outDir </> uBaseName
  let ocamlFile = uOutFile <.> "ml"
  let haskellFile = uOutFile <.> "hs"
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
          writeFile haskellFile (haskellPreamble uBaseName ++ pprint hs)

haskellPreamble uBaseName =
  "module " ++ uBaseName ++ " where\n\n\
  \import Stream\n\
  \import Control.Monad\n\n"