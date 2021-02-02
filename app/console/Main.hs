module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import qualified Transformer.PrologToMk
import qualified ConsPDApp
import qualified CPDApp

data Action
  = Transform { dir :: Maybe FilePath, file :: FilePath }
  | Parse { dir :: Maybe FilePath, file :: FilePath, useIrinaParser :: Bool }
  | ConsPD
  | CPD
  | Default

dirParser :: Parser FilePath
dirParser = strOption
  (  long "dir"
  <> short 'd'
  <> metavar "DIR"
  <> help "The directory to read files from"
  )

fileParser :: Parser FilePath
fileParser = strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Read the miniKanren program from DIR/FILENAME"
  )

transformAction :: Parser Action
transformAction = Transform <$> optional dirParser <*> fileParser

parseParser :: Parser Bool
parseParser = flag' True
  (  long "parse"
  <> short 'p'
  <> help "Run Irina's parser"
  )

parseAction :: Parser Action
parseAction = Parse <$> optional dirParser <*> fileParser <*> parseParser

defaultAction :: Parser Action
defaultAction = flag' Default
  (  long "default"
  <> help "Run the default action"
  )

consPDAction :: Parser Action
consPDAction = flag' ConsPD
  (  long "conspd"
  <> short 'c'
  <> help "Run the consPD action"
  )

cpdAction :: Parser Action
cpdAction = flag' CPD
  (  long "cpd"
  <> help "Run the CPD action"
  )

actionParser :: Parser Action
actionParser =
  transformAction <|>
  parseAction <|>
  consPDAction <|>
  cpdAction <|>
  defaultAction

main :: IO ()
main = runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      ( fullDesc
     <> progDesc "Various transformers for miniKanren programs"
     <> header "uKanren-tranformations" )

runAction :: Action -> IO ()
runAction (Transform directory file) =
    Transformer.PrologToMk.transform (fromMaybe defaultDirectory directory) file
  where
    defaultDirectory = "/home/ev/prj/geoff/cpd/examples/"
runAction (Parse directory file False) =
    runAction (Transform directory file)
runAction (Parse directory file True) =
    print "Not implemented"
runAction ConsPD = ConsPDApp.run
runAction CPD = CPDApp.run
runAction Default = putStrLn "Default action"