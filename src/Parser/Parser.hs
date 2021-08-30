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
import qualified Data.Set as Set
import Control.Monad.State
    ( modify, evalStateT, MonadIO(liftIO), MonadState(get), StateT )
import Text.Printf ( printf )
import Control.Monad.Except ( ExceptT(..), runExceptT )
import Data.List.NonEmpty ( NonEmpty (..) )
import Data.Semigroup ( Semigroup(sconcat) )
import Control.Applicative.Lift ( eitherToErrors, runErrors )

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

-- parseFromFile :: Parser a -> FilePath -> ExceptT (ParserError String) IO Syntax.Program
-- parseFromFile parser filePath = do
--     exist <- liftIO $ doesPathExist filePath
--     if not exist
--     then throwError $ FileNotFound filePath
--     else do
--       content <- liftIO $ readFile filePath
--       return $ runBundlingParserExcept parser content

-- runBundlingParserExcept :: (Stream s, ShowErrorComponent e) => Parsec e s b -> s -> ExceptT (ParserError String) Identity b
-- runBundlingParserExcept parser =
--     mapLeft (SyntaxError . errorBundlePretty) . runParser parser ""

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
    evalStateT (runExceptT $ go path) Set.empty
  where
    go :: FilePath -> ExceptT (ParserError String) (StateT (Set.Set FilePath) IO) Syntax.Program
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
          Right ps -> Right (sconcat $ program :| ps)
