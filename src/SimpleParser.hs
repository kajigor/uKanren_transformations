{-# LANGUAGE DerivingStrategies #-}
module SimpleParser where

import           Control.Monad                  (void)
import           Data.Void                      (Void(..))
import           Data.Either                    (either, fromRight)
import           Eval                           (postEval)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

import           Syntax


defsAsts :: String -> [Def]
defsAsts = fromRight [] . runParser (many parseDef) ""

strDefAsts :: String -> String
strDefAsts = unlines . fmap ((++ "\n") . show) . defsAsts

defAst :: String -> Maybe Def
defAst = either (const Nothing) Just . runParser parseDef ""

runBundlingParser :: (Stream s, ShowErrorComponent e) => Parsec e s b -> s -> Either String b
runBundlingParser parser =
    mapLeft errorBundlePretty . runParser parser ""
  where
    mapLeft f = either (Left . f) Right


parseDefs :: String -> Either String [Def]
parseDefs = runBundlingParser (many parseDef)


parseWholeProgram :: String -> Either String Program
parseWholeProgram = runBundlingParser parseProg

-- Parses the list of relation definitions, expects a goal to evaluate
progAst :: String -> Maybe Program
progAst = either (const Nothing) Just . runParser parseProg ""

strProgAstWithDefGoal :: String -> String
strProgAstWithDefGoal = either errorBundlePretty show . runParser parseProg ""

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

sugar :: [String]
sugar = ["trueo", "falso", "zero", "succ", "conde"]

kw :: [String] 
kw = ["fresh", "in"]

reserved :: [String]
reserved = kw ++ sugar 

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

parseBoolTerm :: Parser (Term X)
parseBoolTerm = try $ falso <|> trueo 

trueo :: Parser (Term X)
trueo = do
  symbol "trueo"
  return $ C "true" []

falso :: Parser (Term X)
falso = do
  symbol "falso"
  return $ C "false" []

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
  <|> parseBoolTerm 

parseConst :: Parser (Term X)
parseConst = 
  C <$> try constructorName <*> return []

parseVar :: Parser (Term X )
parseVar = 
  V <$> try ident 

parseSimpleGoal :: Parser (G X) 
parseSimpleGoal = 
  try $ parseUnification <|> parseInvokation <|> roundBr parseSimpleGoal

parseUnification :: Parser (G X)
parseUnification = try $ do 
  l <- parseTerm 
  symbol "=="
  r <- parseTerm 
  return (l :=: r)

parseInvokation :: Parser (G X)
parseInvokation = parseInvoke

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