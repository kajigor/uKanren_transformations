module Parser.Parser  ( importsParser, parser, ParserType(..), ParserError(..) ) where

import qualified Parser.IrinaParser as IrinaParser
import qualified Parser.SimpleParser as SimpleParser
import Parser.Data ( Parser, ParserError(..), ParserType(..) )
import Text.Megaparsec
    ( Parsec,
      runParser,
      errorBundlePretty,
      ShowErrorComponent,
      Stream )
import Util.Miscellaneous ( mapLeft )
import System.Directory ( doesPathExist )
import System.FilePath ( replaceBaseName )
import qualified Syntax
import qualified Data.Set as Set
import Control.Monad.State
    ( modify, evalStateT, MonadIO(liftIO), MonadState(get), StateT )
import Text.Printf ( printf )
import Data.Either ( isRight, rights, lefts )

parser :: ParserType -> String -> Either (ParserError String) Syntax.Program
parser =
    runBundlingParser . chooseParser
  where
    chooseParser Irina = IrinaParser.parseProg
    chooseParser Simple = SimpleParser.parseProg

importsParser :: ParserType -> FilePath -> IO (Either (ParserError String) Syntax.Program)
importsParser =
    parseImports . chooseParser
  where
    chooseParser Irina = IrinaParser.parseProgramWithImports
    chooseParser Simple = SimpleParser.parseProgramWithImports

parseFromFile :: Parser a -> FilePath -> IO (Either (ParserError String) a)
parseFromFile parser filePath = do
    exist <- doesPathExist filePath
    if not exist
    then return $ Left $ FileNotFound filePath
    else do
      content <- readFile filePath
      return $ runBundlingParser parser content

runBundlingParser :: (Stream s, ShowErrorComponent e) => Parsec e s b -> s -> Either (ParserError String) b
runBundlingParser parser =
    mapLeft (SyntaxError . errorBundlePretty) . runParser parser ""


parseImports :: Parser ([String], Syntax.Program)
             -> FilePath
             -> IO (Either (ParserError String) Syntax.Program)
parseImports parser path = do
    evalStateT (go path) Set.empty
  where
    go :: FilePath -> StateT (Set.Set FilePath) IO (Either (ParserError String) Syntax.Program)
    go filePath = do
      parsingResult <- liftIO $ parseFromFile parser filePath
      case parsingResult of
        Left err -> return $ Left err
        Right (imports, program@(Syntax.Program defs goal)) -> do
          let paths = map (replaceBaseName filePath) imports
          modify (Set.insert filePath)
          seen <- get
          let newImports = filter (`notElem` seen) paths
          if null newImports
          then return $ Right program
          else do
            mapM_ (modify . Set.insert) newImports
            imported <- mapM (\newPath -> do
                newResult <- go newPath
                case newResult of
                  Left err ->
                    return $ Left $ (\x -> printf "Failed to parse %s\n%s" newPath x :: String) <$> err
                  Right (Syntax.Program ds _) -> do
                    return $ Right ds
              ) newImports
            if all isRight imported
            then return $ Right (Syntax.Program (defs ++ concat (rights imported)) goal)
            else return $ Left $ SyntaxError (printf "Failed to parse imports\n%s\n" (show $ lefts imported))
