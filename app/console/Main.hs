module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import qualified Transformer.PrologToMk
import qualified ConsPDApp

data Action
  = Parse { dir :: Maybe FilePath, file :: FilePath}
  | ConsPD
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
  <> help "Parse the miniKanren program located in DIR/FILENAME"
  )

parseAction :: Parser Action
parseAction = Parse <$> optional dirParser <*> fileParser

defaultAction :: Parser Action
defaultAction = flag' Default
  (  long "default"
  <> short 'd'
  <> help "Run the default action"
  )

consPDAction :: Parser Action
consPDAction = flag' ConsPD
  (  long "conspd"
  <> short 'c'
  <> help "Run the consPD action"
  )

actionParser :: Parser Action
actionParser =
  parseAction <|>
  consPDAction <|>
  defaultAction

main :: IO ()
main = runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      ( fullDesc
     <> progDesc "Various transformers for miniKanren programs"
     <> header "uKanren-tranformations" )

runAction :: Action -> IO ()
runAction (Parse directory file) =
    Transformer.PrologToMk.transform (fromMaybe defaultDirectory directory) file
  where
    defaultDirectory = "/home/ev/prj/geoff/cpd/examples/"
runAction ConsPD = ConsPDApp.run
runAction Default = putStrLn "Default action"