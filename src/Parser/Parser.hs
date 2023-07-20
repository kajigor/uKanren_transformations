{-# LANGUAGE ScopedTypeVariables #-}
module Parser.Parser  ( importsParser, parser, ParserType(..), ParserError(..), parseImports ) where

import           Control.Applicative.Lift (eitherToErrors, runErrors)
import           Control.Monad.Except     (ExceptT (..), runExceptT)
import           Control.Monad.State      (MonadIO (liftIO), MonadState (get), StateT, evalStateT, modify)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Semigroup           (Semigroup (sconcat))
import qualified Data.Set                 as Set
import           Parser.Data              (Parser, ParserError (..), ParserType (..))
import qualified Parser.IrinaParser       as IrinaParser
import qualified Parser.SimpleParser      as SimpleParser
import           Program
import qualified Syntax
import           System.Directory         (doesPathExist)
import           System.FilePath          (replaceBaseName)
import           Text.Megaparsec          (Parsec, ShowErrorComponent, errorBundlePretty, runParser)
import           Text.Megaparsec.Stream
import           Text.Printf              (printf)
import           Util.Miscellaneous       (mapLeft)

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

runBundlingParser :: (Stream s, ShowErrorComponent e, VisualStream s, TraversableStream s) => Parsec e s b -> FilePath -> s -> Either (ParserError String) b
runBundlingParser parser filePath =
    mapLeft (SyntaxError . errorBundlePretty) . runParser parser filePath

parseImports :: (Functor (t Syntax.G), Semigroup (t Syntax.G Syntax.X)) => Parser ([String], t Syntax.G Syntax.X)
             -> FilePath
             -> IO (Either (ParserError String) (t Syntax.G Syntax.X))
parseImports parser path = do
    evalStateT (runExceptT $ go parser path) Set.empty
  where
    go :: (Functor (t Syntax.G), Semigroup (t Syntax.G Syntax.X)) => Parser ([String], t Syntax.G Syntax.X) -> 
      FilePath -> ExceptT (ParserError String) (StateT (Set.Set FilePath) IO) (t Syntax.G Syntax.X)
    go parser filePath = do
      (imports, program) <- ExceptT <$> liftIO $ parseFromFile parser filePath
      let paths = map (replaceBaseName filePath) imports
      modify (Set.insert filePath)
      seen <- get
      let newImports = filter (`notElem` seen) paths
      ExceptT $ do
        mapM_ (modify . Set.insert) newImports
        imported <- runErrors . sequenceA <$> mapM (\newPath -> do
            newResult <- runExceptT $ go parser newPath
            eitherToErrors <$> case newResult of
              Left err ->
                return $ Left [(\x -> printf "Failed to parse %s\n%s" newPath x :: String) <$> err]
              Right p -> return $ Right p
          ) newImports
        return $ case imported of
          Left errs -> Left $ SyntaxError (printf "Failed to parse imports\n%s\n" (show errs))
          Right ps -> Right (sconcat (program :| ps))

