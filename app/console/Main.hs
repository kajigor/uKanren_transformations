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
import Util.Miscellaneous (mapLeft)
import qualified Parser.Parser as Parser
import Syntax (Program)
import Text.Printf (printf)

data Transformation
  = CPD
  | ConsPD
  | Parser
  | Eval
  | Normalize
  | PrologToMk

data Action = Action { transformation :: Transformation
                     , input :: FilePath
                     , output :: FilePath
                     , isInputADir :: Bool
                     , parserType :: Parser.ParserType
                     , numAnswers :: Int
                     }

data Args = Args Transformation (Maybe FilePath) (Maybe FilePath) (Maybe Parser.ParserType) Int

transform :: Args -> IO Action
transform (Args transformation input output parserType numAnswers) = do
    curDir <- getCurrentDirectory
    let i = fromMaybe curDir input
    failIfNotExist i
    isInputADir <- isDir i

    let defaultOutput = defaultOutputDir transformation
    let out = fromMaybe defaultOutput output
    -- forM_ output createDirRemoveExisting

    let pType = fromMaybe Parser.Simple parserType

    return $ Action transformation i out isInputADir pType numAnswers

actionParser :: Parser Args
actionParser =
  Args  <$> parseTransformation
        <*> optional inputParser
        <*> optional outputParser
        <*> optional parserTypeParser
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

parserTypeParser :: Parser Parser.ParserType
parserTypeParser = flag Parser.Simple Parser.Irina
  (  long "irina"
  <> short 'p'
  <> help "Run Irina's parser"
  )

parseTransformation :: Parser Transformation
parseTransformation =
      consPDParser
  <|> cpdParser
  <|> parserParser
  <|> evalParser
  <|> normalizeParser
  <|> prologToMkParser

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

prologToMkParser :: Parser Transformation
prologToMkParser = flag' PrologToMk
  (  long "pr2mk"
  <> help "Run prolog to miniKanren transformation"
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

chooseParser :: Parser.ParserType -> (String -> IO (Either String Program))
chooseParser pType = \input -> do
  res <- Parser.importsParser pType input
  return $ mapLeft show res

chooseTransformer CPD = CPDApp.runWithParser
chooseTransformer ConsPD = ConsPDApp.runWithParser

defaultOutputDir args =
  printf "test/out/%s" $
    case args of
      CPD -> "cpd"
      ConsPD -> "consPD"
      Eval -> "eval"
      Normalize -> "norm"
      Parser -> "parse"

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  let parser = chooseParser $ parserType action
  case transformation action of
    Eval ->
      EvalApp.runWithParser parser (input action) (numAnswers action)
    Normalize ->
      NormalizeApp.runWithParser parser (input action)
    Parser ->
      ParseApp.run parser (input action)
    PrologToMk ->
      Transformer.PrologToMk.transform (input action)
    x -> do
      let transformer = chooseTransformer (transformation action)
      if isInputADir action
      then do
        files <- getFiles "mk" (input action)
        mapM_ (transformer parser (output action)) files
      else
        transformer parser (output action) (input action)
