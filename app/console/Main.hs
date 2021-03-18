module Main where

import Options.Applicative
import Data.Foldable (forM_)
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe, fromJust, isNothing)
import qualified EvalApp
import qualified Transformer.PrologToMk
import System.Directory (getCurrentDirectory)
import qualified ConsPDApp
import qualified CPDApp
import qualified ParseApp
import qualified NormalizeApp
import Util.File (failIfNotExist, isDir, getFiles, createDirRemoveExisting)

data Transformation
  = CPD
  | ConsPD
  | Parser
  | Eval
  | Normalize

data Action = Action { transformation :: Transformation
                     , input :: FilePath
                     , output :: Maybe FilePath
                     , isInputADir :: Bool
                     , useIrinaParser :: Bool
                     , numAnswers :: Int
                     }

data Args = Args Transformation (Maybe FilePath) (Maybe FilePath) (Maybe Bool) Int

transform :: Args -> IO Action
transform (Args transformation input output useIrinaParser numAnswers) = do
    curDir <- getCurrentDirectory
    let i = fromMaybe curDir input
    failIfNotExist i
    isInputADir <- isDir i

    -- forM_ output createDirRemoveExisting

    return $ Action transformation i output isInputADir (toBool useIrinaParser) numAnswers
  where
    toBool x = isNothing x || fromJust x


actionParser :: Parser Args
actionParser =
  Args  <$> parseTransformation
        <*> optional inputParser
        <*> optional outputParser
        <*> optional kindOfParserParser
        <*> numAnswersParser

numAnswersParser :: Parser Int
numAnswersParser = option auto
  (  short 'n'
  <> help "How many answers to get"
  <> showDefault
  <> value 1
  <> metavar "INT" )

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
  consPDParser <|> cpdParser <|> parserParser <|> evalParser <|> normalizeParser

normalizeParser :: Parser Transformation
normalizeParser = flag' Normalize
  (  long "norm"
  <> help "Normalize the program"
  )

consPDParser :: Parser Transformation
consPDParser = flag' ConsPD
  (  long "conspd"
  <> help "Run the consPD transformation"
  )

evalParser :: Parser Transformation
evalParser = flag' Eval
  (  long "eval"
  <> help "Evaluate the goal"
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
    Eval | useIrinaParser action ->
      EvalApp.runWithParser (input action) (numAnswers action)
    Normalize | useIrinaParser action ->
      NormalizeApp.run (input action)
    Parser ->
      if useIrinaParser action
      then ParseApp.run (input action)
      else
        Transformer.PrologToMk.transform (input action)
    x | useIrinaParser action -> do
      let transformer = case x of CPD -> CPDApp.runWithParser; ConsPD -> ConsPDApp.runWithParser
      let out = fromMaybe "test/out/cpd" (output action)
      if isInputADir action
      then do
        files <- getFiles "mk" (input action)
        mapM_ (transformer out) files
      else
        transformer out (input action)
    CPD ->
      CPDApp.run
    ConsPD ->
      ConsPDApp.run

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