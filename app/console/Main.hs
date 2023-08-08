module Main where

import qualified ConsPDApp
import qualified CPDApp
import           Data.Maybe             (fromMaybe)
import qualified EvalApp
import qualified ModeApp
import qualified NormalizeApp
import qualified TerminationCheckApp
import qualified Parser.AnnotatedParser as AnnotatedParser
import           Options.Applicative
import qualified ParseApp
import qualified Parser.Parser          as Parser
import           Program
import           BTA.AnnotatedProgram
import           Syntax                 (G, X)
import           System.Directory       (getCurrentDirectory)
import           Text.Printf            (printf)
import qualified Transformer.PrologToMk
import qualified TranslateApp
import           Util.File              (failIfNotExist, getFiles, isDir)
import           Util.Miscellaneous     (mapLeft)

data Transformation
  = CPD
  | ConsPD
  | Parser
  | Eval
  | Normalize
  | PrologToMk
  | Mode
  | Translate
  | TerminationCheck

data Action = Action { transformation :: Transformation
                     , input          :: FilePath
                     , output         :: FilePath
                     , isInputADir    :: Bool
                     , parserType     :: Parser.ParserType
                     , numAnswers     :: Int
                     , groundVars     :: Maybe [Int]
                     , relName        :: String
                     }

data Args = Args
  { transformationArg :: Transformation
  , inputArg          :: Maybe FilePath
  , outputArg         :: Maybe FilePath
  , parserTypeArg     :: Maybe Parser.ParserType
  , numAnswersArg     :: Int
  , groundVarsArg     :: Maybe [Int]
  , relNameArg        :: String
  }

transform :: Args -> IO Action
transform (Args transformation input output parserType numAnswers groundVars relName) = do
    curDir <- getCurrentDirectory
    let i = fromMaybe curDir input
    failIfNotExist i
    isInputADir <- isDir i

    let defaultOutput = defaultOutputDir transformation
    let out = fromMaybe defaultOutput output
    -- forM_ output createDirRemoveExisting

    let pType = fromMaybe Parser.Simple parserType 
    
    return $ Action transformation i out isInputADir pType numAnswers groundVars relName

actionParser :: Parser Args
actionParser =
  Args  <$> parseTransformation
        <*> optional inputParser
        <*> optional outputParser
        <*> optional parserTypeParser
        <*> numAnswersParser
        <*> optional groundVarsParser
        <*> relationNameParser

numAnswersParser :: Parser Int
numAnswersParser = option auto
  (  short 'n'
  <> help "How many answers to get"
  <> showDefault
  <> value 1
  <> metavar "INT" )

groundVarsParser :: Parser [Int]
groundVarsParser = option auto
  (  long "ground"
  <> help "Which variables are supposed to be treated as ground"
  <> showDefault
  <> metavar "GROUND")

relationNameParser :: Parser String
relationNameParser = strOption
  (  long "rel"
  <> help "Which relation to mode-analyze"
  <> showDefault
  <> value ""
  <> metavar "REL")

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
  <|> modeParser
  <|> translateParser
  <|> terminationCheckParser

terminationCheckParser :: Parser Transformation
terminationCheckParser = flag' TerminationCheck
  (  long "terminationCheck"
  <> help "run check on safety unfolding"
  )

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

modeParser :: Parser Transformation
modeParser = flag' Mode
  (  long "mode"
  <> help "Run mode analysis"
  )

translateParser :: Parser Transformation
translateParser = flag' Translate
  ( long "translate"
  <> help "Translate miniKanren to Haskell"
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

-- main = testTrans

main :: IO ()
main = do
    runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "Various transformers for miniKanren programs"
      <> header "uKanren-tranformations"
      )

chooseParser :: Parser.ParserType -> (String -> IO (Either String (Program G X)))
chooseParser pType input = do
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
      Mode -> "mode"
      Translate -> "translate"

getAnnotationParser :: (String -> IO (Either String (AnnotatedProgram G X)))
getAnnotationParser input = do 
  res <- Parser.parseImports AnnotatedParser.parseProgramWithImports input
  return $ mapLeft show res

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  let parser = chooseParser $ parserType action
  case transformation action of
    Eval ->
      EvalApp.runWithParser parser (input action) (numAnswers action)
    Mode ->
      ModeApp.runWithParser parser (input action) (relName action) (fromMaybe [] $ groundVars action)
    Normalize ->
      NormalizeApp.runWithParser parser (input action)
    Parser ->
      ParseApp.run parser (input action)
    PrologToMk ->
      Transformer.PrologToMk.transform (input action)
    Translate ->
      TranslateApp.runWithParser parser (input action) (output action) (relName action) (groundVars action)
    TerminationCheck -> 
      TerminationCheckApp.runWithParser (getAnnotationParser) (input action) (output action)
    x -> do
      let transformer = chooseTransformer (transformation action)
      if isInputADir action
      then do
        files <- getFiles "mk" (input action)
        mapM_ (transformer parser (output action) (groundVars action)) files
      else
        transformer parser (output action) (groundVars action) (input action)
