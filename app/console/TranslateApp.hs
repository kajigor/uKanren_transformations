{-# LANGUAGE TupleSections #-}

module TranslateApp where

import           Data.List                 (find, subsequences)
import           Def
import           FunConversion.OCamlPretty (prettyString)
import           FunConversion.Syntax      (embedProgSafe)
import           FunConversion.Trans       (transMultiMode, transProg)
import           Language.Haskell.TH       (pprint)
import           Program
import           Syntax
import           System.Directory          (copyFile)
import           System.FilePath           (takeBaseName, (<.>), (</>))
import           Util.File                 (createDirRemoveExisting)
import           Util.String
import qualified Transformer.MkToProlog

runWithParser :: (FilePath -> IO (Either String (Program G String))) -> FilePath -> FilePath -> String -> Maybe [Int] -> IO ()
runWithParser parser inputFile outDir relName inputs = do
  createDirRemoveExisting outDir
  let baseName = takeBaseName inputFile
  let uBaseName = toUpper baseName
  let outFile = outDir </> baseName
  let uOutFile = outDir </> uBaseName
  let ocamlFile = uOutFile <.> "ml"
  let haskellFile = uOutFile <.> "hs"
  let prologFile = uOutFile <.> "pl"
  copyFile inputFile (outFile <.> "mk")

  program <- parser inputFile
  case program of
    Right program -> do
      Transformer.MkToProlog.transform prologFile (getDefs program)
      case inputs of
        Just inputs -> do
          let translatedProgram = transProg relName inputs program
          outputProgram ocamlFile haskellFile uBaseName translatedProgram
        Nothing -> do
          let def = find (\def -> getName def == relName) (getDefs program)
          case def of
            Just def -> do
              let inputs = subsequences [0 .. length (getArgs def) - 1]
              let translatedProgram = transMultiMode (getDefs program) (map (relName,) inputs)
              outputProgram ocamlFile haskellFile uBaseName translatedProgram
            Nothing -> putStrLn $ relName ++ " undefined"
    Left err -> putStrLn $ "Parsing error: " ++ err
  where
    outputProgram ocamlFile haskellFile uBaseName program = do
      case program of
        Left err -> putStrLn $ "Error in translating: " ++ err
        Right hs -> do
          let ocamlPr = prettyString hs
          writeFile ocamlFile ocamlPr

          let pr = embedProgSafe relName hs
          case pr of
            Left err -> putStrLn $ "Template Haskell error: " ++ err
            Right hs ->
              writeFile haskellFile (haskellPreamble uBaseName ++ pprint hs)

haskellPreamble :: String -> String
haskellPreamble uBaseName =
  "module " ++ uBaseName ++ " where\n\n\
  \import Stream\n\
  \import Control.Monad\n\n"
