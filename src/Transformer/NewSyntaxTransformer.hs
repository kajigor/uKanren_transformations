module Transformer.NewSyntaxTransformer where

import qualified Parser.SimplePretty as PP
import qualified Parser.Parser as P
import Parser.Data
import Text.Printf
import           Util.Miscellaneous     (mapLeft)
import Program
import Syntax
import TranslatedExamples.BridgeKarnen
import TranslatedExamples.BridgeTerm
import TranslatedExamples.Einstein
import TranslatedExamples.EinsteinKarnen
import TranslatedExamples.EvalLoop
import TranslatedExamples.WGC
import TranslatedExamples.WGCKarnen

chooseParser pType input = do
  res <- P.importsParser pType input
  return $ mapLeft show res

oldParser = chooseParser Irina

newParser = chooseParser Simple

transformOldSyntax :: FilePath -> FilePath -> IO ()
transformOldSyntax input output = do
  parsed <- oldParser input
  case parsed of
    Left err -> putStrLn err
    Right prog -> do
      writeFile output (PP.prettyString prog)

      result <- newParser output
      case result of
        Left err -> do
          putStrLn (printf "Failed to transform %s\nError: %s\nSee %s" input err output)
        Right _ -> return ()

main :: IO ()
main = do
  justPrint bridgeProgram "test/resources/newSyntax/bridgeFromMK.mk"
  justPrint einsteinProgram "test/resources/newSyntax/einsteinFromMK.mk"
  justPrint wgcProgram "test/resources/newSyntax/wgcFromMK.mk"

justPrint :: Program G X -> FilePath -> IO ()
justPrint program output = do
  writeFile output (PP.prettyString program)
  result <- newParser output
  case result of
    Left err -> do
      putStrLn (printf "Failed to transform\nError: %s\nSee %s" err output)
    Right p | p == program -> return ()
            | otherwise -> putStrLn (printf "Failed to transform: different program.\nBefore:\n%s\n\nAfter:\n%s\nSee %s" (PP.prettyString program) (PP.prettyString p) output)
