{-
  This file is created by Irina Artemeva
  Available at https://github.com/Pluralia/uKanren_translator/blob/58ab786f94f68da1027428706448fbddb980fa72/src/Parser.hs
-}

{-# LANGUAGE DerivingStrategies #-}
module Parser (
      progAst
    , strProgAstWithDefGoal
    , defsAsts
    , strDefAsts
    , defAst
    , parseDefs
    , parseWholeProgram
    ) where

import           Control.Monad                  (void)
import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Data.Void                      (Void(..))
import           Data.Either                    (either, fromRight)
import           Eval                           (postEval)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Syntax

-----------------------------------------------------------------------------------------------------

defsAsts :: String -> [Def]
defsAsts = fromRight [] . runParser (many parseDef) ""

strDefAsts :: String -> String
strDefAsts = unlines . fmap ((++ "\n") . show) . defsAsts

defAst :: String -> Maybe Def
defAst = either (const Nothing) Just . runParser parseDef ""

-----------------------------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------------------------------

type Parser = Parsec Void String

-----------------------------------------------------------------------------------------------------

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

ident :: Parser String
ident = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (char '_' <|> alphaNumChar <|> char '\'')
    check x = if x `elem` sugar
                then fail $ show x ++ " cannot be an identifier"
                else return x

-- brackets
roundBr, angleBr, boxBr, curvyBr :: Parser a -> Parser a
roundBr = between (symbol "(") (symbol ")")
angleBr = between (symbol "<") (symbol ">")
boxBr   = between (symbol "[") (symbol "]")
curvyBr = between (symbol "{") (symbol "}")

-----------------------------------------------------------------------------------------------------

-- term
parseTerm :: Parser (Term X)
parseTerm = parseListTerm


parseListTerm :: Parser (Term X)
parseListTerm = try conso
  <|> try (roundBr conso)
  <|> niloOrNum

niloOrNum :: Parser (Term X)
niloOrNum = try parseNumTerm <|> do
  symbol "[]"
  return $ C "Nil" []

conso :: Parser (Term X)
conso = do
  x  <- niloOrNum <|> roundBr parseTerm
  xs <- some $ symbol "%" *> parseTerm
  return $ foldr1 (\term acc -> C "Cons" [term, acc]) $ (x : xs)


parseNumTerm :: Parser (Term X)
parseNumTerm = try succo
  <|> try (roundBr succo)
  <|> zeroOrBool

zeroOrBool :: Parser (Term X)
zeroOrBool = try parseBoolTerm <|> do
  symbol "zero"
  return $ C "O" []

succo :: Parser (Term X)
succo = do
  symbol "succ"
  x <- try zeroOrBool <|> roundBr succo
  return $ C "S" [x]


parseBoolTerm :: Parser (Term X)
parseBoolTerm = try falso
  <|> try trueo
  <|> parseDesugarTerm

trueo :: Parser (Term X)
trueo = do
  symbol "trueo"
  return $ C "true" []

falso :: Parser (Term X)
falso = do
  symbol "falso"
  return $ C "false" []


parseDesugarTerm :: Parser (Term X)
parseDesugarTerm = try c <|> v
  where
    v = V <$> ident
    c = angleBr $ do
      name  <- ident
      symbol ":"
      terms <- many parseTerm
      return $ C name terms

-----------------------------------------------------------------------------------------------------

-- goal
parseFresh :: Parser (G X)
parseFresh = boxBr $ do
  names <- some ident
  symbol ":"
  goal <- parseGoal
  return $ foldr Fresh goal names

parseInvoke :: Parser (G X)
parseInvoke = curvyBr $
      Invoke
  <$> ident
  <*> many parseTerm

parsePat :: Parser (G X)
parsePat = try $ do
      term1 <- parseTerm
      symbol "==="
      term2 <- parseTerm
      return $ term1 :=: term2
  <|> try (roundBr parseOp)
  <|> try parseFresh
  <|> parseInvoke

parseOp :: Parser (G X)
parseOp =
  unsafeDisj' <$> NE.sepBy1 (unsafeConj' <$> NE.sepBy1 parsePat (symbol "/\\")) (symbol "\\/")

  -- makeExprParser parsePat ops
  -- where
  --   go assoc op f = assoc (f <$ symbol op)
  --   ops = [ [ go InfixR "/\\" (:/\:) ]
  --         , [ go InfixR "\\/" (:\/:) ]
  --         ]

parseGoal :: Parser (G X)
parseGoal = try parseConde
  <|> parseDesugarGoal

parseConde :: Parser (G X)
parseConde = do
  symbol "conde"
  goals <- some (roundBr parseDesugarGoal)
  return $ unsafeDisj goals

parseDesugarGoal :: Parser (G X)
parseDesugarGoal = sc
   *> try parseOp
  <|> try parseFresh
  <|> parseInvoke

parseQuery :: Parser (G X)
parseQuery = do
  symbol "?"
  parseGoal

-----------------------------------------------------------------------------------------------------

-- definition
parseDef :: Parser Def
parseDef = do
  symbol "::"
  name <- ident
  args <- many ident
  symbol "="
  goal <- parseGoal
  return $ Def name args goal

-----------------------------------------------------------------------------------------------------

-- program
parseProg :: Parser Program
parseProg =
  Program <$> many parseDef <*> (postEval [] <$> parseQuery)