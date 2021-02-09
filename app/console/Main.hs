module Main where

import Options.Applicative
import Data.Foldable (forM_)
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe, fromJust, isNothing)
import qualified Transformer.PrologToMk
import System.Directory (getCurrentDirectory)
import qualified ConsPDApp
import qualified CPDApp
import qualified ParseApp
import Util.File (failIfNotExist, isDir, failIfNotDir, getFiles, createDirRemoveExisting)

-- data Action
--   = Transform { dir :: Maybe FilePath, file :: FilePath }
--   | Parse { dir :: Maybe FilePath, file :: FilePath, useIrinaParser :: Bool }
--   | ConsPD
--   | CPD
--   | Default

data Transformation
  = CPD
  | ConsPD
  | Parser

data Action = Action { transformation :: Transformation
                     , input :: FilePath
                     , output :: Maybe FilePath
                     , isInputADir :: Bool
                     , useIrinaParser :: Bool
                     }

data Args = Args Transformation (Maybe FilePath) (Maybe FilePath) (Maybe Bool)

transform :: Args -> IO Action
transform (Args transformation input output useIrinaParser) = do
    curDir <- getCurrentDirectory
    let i = fromMaybe curDir input
    failIfNotExist i
    isInputADir <- isDir i

    forM_ output createDirRemoveExisting

    return $ Action transformation i output isInputADir (toBool useIrinaParser)
  where
    toBool x = isNothing x || fromJust x


actionParser :: Parser Args
actionParser =
  Args  <$> parseTransformation
        <*> optional inputParser
        <*> optional outputParser
        <*> optional kindOfParserParser

inputParser :: Parser FilePath
inputParser = strOption
  (  long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> help "Where to read input from. If it is a directory, all files in it are transformed."
  )

outputParser :: Parser FilePath
outputParser = strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "Where to put the transformation results."
  )

kindOfParserParser :: Parser Bool
kindOfParserParser = flag' True
  (  long "irina"
  <> short 'p'
  <> help "Run Irina's parser"
  )

parseTransformation :: Parser Transformation
parseTransformation =
  consPDParser <|> cpdParser <|> parserParser

consPDParser :: Parser Transformation
consPDParser = flag' ConsPD
  (  long "conspd"
  <> help "Run the consPD transformation"
  )

cpdParser :: Parser Transformation
cpdParser = flag' CPD
  (  long "cpd"
  <> help "Run the CPD transformation"
  )

parserParser :: Parser Transformation
parserParser = flag' Parser
  (  long "parser"
  <> help "Run parser"
  )

main :: IO ()
main = do
  runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "Various transformers for miniKanren programs"
      <> header "uKanren-tranformations"
      )

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  case transformation action of
    CPD | useIrinaParser action -> do
      let out = fromMaybe "test/out/cpd" (output action)
      if isInputADir action
      then do
        files <- getFiles "mk" (input action)
        mapM_ (CPDApp.runWithParser out) files
      else
        CPDApp.runWithParser out (input action)
    CPD ->
      CPDApp.run
    ConsPD -> ConsPDApp.run
    Parser ->
      if useIrinaParser action
      then ParseApp.run (input action)
      else
        Transformer.PrologToMk.transform (input action)

-- runAction (Transform directory file) =
--     Transformer.PrologToMk.transform (fromMaybe defaultDirectory directory) file
--   where
--     defaultDirectory = "/home/ev/prj/geoff/cpd/examples/"
-- runAction (Parse directory file False) =
--     runAction (Transform directory file)
-- runAction (Parse directory file True) =
--     ParseApp.run directory file
-- runAction ConsPD = ConsPDApp.run
-- runAction CPD = CPDApp.run
-- runAction Default = putStrLn "Default action"