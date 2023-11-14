{-# LANGUAGE TupleSections #-}

module FunTransformerApp where

import           Program
import           Syntax
import           Data.List
import           BTA.AnnotatedProgram
import           BTA.AnnotatedDef
import qualified Def
import           System.Directory          (copyFile)
import           System.FilePath           (takeBaseName, (<.>), (</>))
import           Util.File                 (createDirRemoveExisting)
import           Util.String
import           CPD.LocalControl          (Heuristic)
import qualified Transformer.CPD           as CPD    
import qualified Transformer.OfflinePD     as Offline
import           Language.Haskell.TH       (pprint)
import           BTA.AnnotationsSetting    (setAnnotations)
import           Printer.PrettyMkPrinter   (prettyMk)
import FunConversion.Trans (transMultiMode)
import FunConversion.Syntax (embedProgSafe)
import FunConversion.OCamlPretty (prettyString)
import Transformer.ConsPD (haskellPreamble)
import Debug.Trace

data Deduction =
  Offline | 
  Online 
  deriving (Show, Read)


runWithParser :: (FilePath -> IO (Either String (AnnotatedProgram G String))) -> FilePath -> FilePath -> Heuristic -> Deduction -> IO ()
runWithParser parser inputFile outDir heuristic deduction = do
  createDirRemoveExisting outDir
  let baseName = takeBaseName inputFile
  let uBaseName = toUpper baseName
  let outFile = outDir </> baseName </> baseName
  let uOutFile = outDir </> baseName </> uBaseName
  let haskellFile = uOutFile <.> "hs"
  let ocamlFile = uOutFile <.> "ml"
  
  parsed <- parser inputFile
  
  case parsed of
    Left err ->
      putStrLn err
    Right program -> do 
      let simpleProgram = convertToSimplePr program
      deduced@(Program defs goal) <- 
        case deduction of 
          Offline -> do
            let annotatedProgram = setAnnotations $ annotateInvokesPr program 
            res <- Offline.transform' outDir baseName annotatedProgram Nothing heuristic
            writeFile (outFile <.> "deduced") $ prettyMk res    
            writeFile (outFile <.> "ann") $ show annotatedProgram
            return res
          Online -> do 
            res <- CPD.transform' outDir baseName simpleProgram Nothing heuristic
            writeFile (outFile <.> "deduced") $ prettyMk res   
            return res 
            
      relName <- getName goal 
        
      let def = find (\def -> Def.getName def == relName) defs
      case def of
        Just def -> do
          let inputs = subsequences [0 .. length (Def.getArgs def) - 1]
          let translatedProgram = transMultiMode defs (map (relName,) inputs)
          outputProgram relName ocamlFile haskellFile uBaseName translatedProgram
        Nothing -> putStrLn $ relName ++ " undefined"
                  
      copyFile inputFile (outFile <.> "mk")
    where
      outputProgram relName ocamlFile haskellFile uBaseName program = do
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
      getName (Invoke name _) = do 
        return name
      getName (Fresh x g) = do 
        getName g 
      
      
      
convertToSimplePr :: AnnotatedProgram G X -> Program G X 
convertToSimplePr pr@(AnnotatedProgram defs goal) = 
  Program (map transnformOneDef defs) goal
            
transnformOneDef :: AnnotatedDef G X -> Def.Def G X 
transnformOneDef (AnnotatedDef name args body _) = Def.Def name args body
         