module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import qualified Transformer.PrologToMk

data Action
  = Parse { dir :: Maybe FilePath, file :: FilePath}
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

actionParser :: Parser Action
actionParser = parseAction <|> defaultAction

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
runAction Default = putStrLn "Default action"