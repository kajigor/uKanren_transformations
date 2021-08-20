{-# LANGUAGE DerivingStrategies #-}
module SimpleParser where

import           Control.Monad                  (void)
import           Data.Void                      (Void(..))
import           Eval                           (postEval)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

import           Syntax
import Util.File
import System.FilePath ( replaceBaseName )
import Data.Either (rights, isRight, lefts)
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace (traceM)

parseImports :: FilePath -> IO (Either String Program)
parseImports path = do
    evalStateT (go path) Set.empty
  where
    go :: FilePath -> StateT (Set.Set String) IO (Either String Program)
    go filePath = do
      parsingResult <- liftIO $ parseFromFile parseProgramWithImports filePath
      case parsingResult of
        Left err -> return $ Left err
        Right (imports, program@(Program defs goal)) -> do
          let paths = map (replaceBaseName filePath) imports
          modify (Set.insert filePath)
          seen <- get
          let newImports = filter (`notElem` seen) paths
          traceM (printf "Seen\n%s\nPaths\n%s\nParsing\n%s\n\n" (show seen) (show paths) (show filePath))
          if null newImports
          then return $ Right program
          else do
            mapM_ (modify . Set.insert) newImports
            imported <- mapM (\newPath -> do
                newResult <- go newPath
                case newResult of
                  Left err ->
                    return $ Left (printf "Failed to parse %s\n%s" newPath err :: String)
                  Right (Program ds _) -> do
                    return $ Right ds
              ) newImports
            if all isRight imported
            then return $ Right (Program (defs ++ concat (rights imported)) goal)
            else return $ Left (printf "Failed to parse imports\n%s\n" (show $ lefts imported))

parseFromFile :: Parser a -> FilePath -> IO (Either String a)
parseFromFile parser filePath = do
    failIfNotExist filePath
    content <- readFile filePath
    return $ runBundlingParser parser content

runBundlingParser :: (Stream s, ShowErrorComponent e) => Parsec e s b -> s -> Either String b
runBundlingParser parser =
    mapLeft errorBundlePretty . runParser parser ""
  where
    mapLeft f = either (Left . f) Right

parseWholeProgram :: String -> Either String Program
parseWholeProgram = runBundlingParser parseProg

parseProgramWithImports :: Parser ([String], Program)
parseProgramWithImports = do
    imports <- many parseImport
    program <- parseProg
    return (imports, program)

parseImport :: Parser String
parseImport = do
  symbol "import"
  ident

type Parser = Parsec Void String

-- spaces & comments
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: [String]
reserved = ["fresh", "in"]

notReserved :: Monad m => String -> m String
notReserved x | x `elem` reserved = fail $ printf "%s is reserved" (show x)
notReserved x = return x

identLetters :: Parser Char
identLetters =
  char '_' <|> alphaNumChar <|> char '\''

ident :: Parser String
ident =
    (lexeme . try) (p >>= notReserved)
  where
    p = (:) <$> lowerChar <*> many identLetters

constructorName :: Parser String
constructorName =
    (lexeme . try) (p >>= notReserved)
  where
    p = (:) <$> upperChar <*> many identLetters

-- brackets
roundBr, angleBr, boxBr, curvyBr :: Parser a -> Parser a
roundBr = between (symbol "(") (symbol ")")
angleBr = between (symbol "<") (symbol ">")
boxBr   = between (symbol "[") (symbol "]")
curvyBr = between (symbol "{") (symbol "}")

comma :: Parser String
comma = symbol ","

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p comma

consList :: Parser (Term X)
consList = do
  head <- parseTerm
  symbol "::"
  tail <- parseVar
  return $ C "Cons" [head, tail]

listTerm :: Parser (Term X)
listTerm = try $
  roundBr consList <|> elemList

termsToList :: [Term X] -> Term X
termsToList = foldr (\h t -> C "Cons" [h, t]) (C "Nil" [])

elemList :: Parser (Term X)
elemList = boxBr $ do
  terms <- commaSep parseTerm
  return $ termsToList terms

parseFresh :: Parser (G X)
parseFresh = do
  symbol "fresh"
  names <- commaSep1 ident
  symbol "in"
  goal <- parseGoal
  return $ foldr Fresh goal names

parseInvoke :: Parser (G X)
parseInvoke =
      Invoke
  <$> try ident
  <*> try parseArguments

parseTerm :: Parser (Term X)
parseTerm =
  C <$> try constructorName <*> try parseArguments
  <|> parseSimpleTerm

parseArguments :: Parser [Term X]
parseArguments = many $ roundBr parseTerm <|> parseSimpleTerm

parseSimpleTerm :: Parser (Term X)
parseSimpleTerm =
      parseVar
  <|> parseConst
  <|> listTerm

parseConst :: Parser (Term X)
parseConst =
  C <$> try constructorName <*> return []

parseVar :: Parser (Term X )
parseVar =
  V <$> try ident

parseSimpleGoal :: Parser (G X)
parseSimpleGoal =
  try $ parseUnification <|> parseInvocation <|> roundBr parseSimpleGoal

parseUnification :: Parser (G X)
parseUnification = try $ do
  l <- parseTerm
  symbol "=="
  r <- parseTerm
  return (l :=: r)

parseInvocation :: Parser (G X)
parseInvocation = parseInvoke

parseOp :: Parser (G X)
parseOp =
  unsafeDisj <$> sepBy1 (unsafeConj <$> sepBy1 (try $ parseSimpleGoal <|> roundBr parseGoal) (symbol "&")) (symbol "|")

parseGoal :: Parser (G X)
parseGoal = sc
   *> try parseOp
  <|> try parseFresh

parseQuery :: Parser (G X)
parseQuery = do
  symbol "?"
  parseGoal

-- definition
parseDef :: Parser Def
parseDef = do
  name <- ident
  args <- many ident
  symbol "="
  goal <- parseGoal
  return $ Def name args goal

-- program
parseProg :: Parser Program
parseProg =
  Program <$> many parseDef <*> (postEval [] <$> parseQuery)