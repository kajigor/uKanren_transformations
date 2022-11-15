{-# LANGUAGE ScopedTypeVariables #-}
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
import Program
import qualified Data.Set as Set
import Control.Monad.State
    ( modify, evalStateT, MonadIO(liftIO), MonadState(get), StateT )
import Text.Printf ( printf )
import Control.Monad.Except ( ExceptT(..), runExceptT )
import Data.List.NonEmpty ( NonEmpty (..) )
import Data.Semigroup ( Semigroup(sconcat) )
import Control.Applicative.Lift ( eitherToErrors, runErrors )

parser :: ParserType -> String -> Either (ParserError String) (Program Syntax.G Syntax.X)
parser pType =
    runBundlingParser (chooseParser pType) ""
  where
    chooseParser Irina = IrinaParser.parseProg
    chooseParser Simple = SimpleParser.parseProg

importsParser :: ParserType -> FilePath -> IO (Either (ParserError String) (Program Syntax.G Syntax.X))
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
      return $ runBundlingParser parser filePath content

runBundlingParser :: (Stream s, ShowErrorComponent e) => Parsec e s b -> FilePath -> s -> Either (ParserError String) b
runBundlingParser parser filePath =
    mapLeft (SyntaxError . errorBundlePretty) . runParser parser filePath

parseImports :: Parser ([String], Program Syntax.G Syntax.X)
             -> FilePath
             -> IO (Either (ParserError String) (Program Syntax.G Syntax.X))
parseImports parser path = do
    evalStateT (runExceptT $ go path) Set.empty
  where
    go :: FilePath -> ExceptT (ParserError String) (StateT (Set.Set FilePath) IO) (Program Syntax.G Syntax.X)
    go filePath = do
      (imports, program) <- ExceptT <$> liftIO $ parseFromFile parser filePath
      let paths = map (replaceBaseName filePath) imports
      modify (Set.insert filePath)
      seen <- get
      let newImports = filter (`notElem` seen) paths
      ExceptT $ do
        mapM_ (modify . Set.insert) newImports
        imported <- runErrors . sequenceA <$> mapM (\newPath -> do
            newResult <- runExceptT $ go newPath
            eitherToErrors <$> case newResult of
              Left err ->
                return $ Left [(\x -> printf "Failed to parse %s\n%s" newPath x :: String) <$> err]
              Right p -> return $ Right p
          ) newImports
        return $ case imported of
          Left errs -> Left $ SyntaxError (printf "Failed to parse imports\n%s\n" (show errs))
          Right ps -> Right (sconcat (program :| ps))

