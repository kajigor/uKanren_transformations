{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
module Parser.SimpleParser where

import           Def
import           Eval            (postEval)
import           Parser.Data     (Parser)
import           Parser.Lexer    (boxBr, comma, lIdentifier, lexeme, roundBr, sc, symbol, uIdentifier)
import           Program
import           Syntax          (G (..), Term (..), X, unsafeConj, unsafeDisj)
import           Text.Megaparsec (MonadParsec (try), many, sepBy, sepBy1, some, (<?>), (<|>))

parseProgramWithImports :: Parser ([String], Program G X)
parseProgramWithImports = sc *> do
    imports <- many parseImport
    program <- parseProg
    return (imports, program)

parseImport :: Parser String
parseImport = do
  lexeme (symbol "import")
  ident

delay :: Parser String
delay = lexeme (symbol "Delay")

reserved :: [String]
reserved = ["fresh", "in", "import", "delay"]

ident :: Parser String
ident = lIdentifier reserved

constructorName :: Parser String
constructorName = uIdentifier reserved

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p comma

consList :: Parser (Term X)
consList = do
  headTerms <- some (try (parseTerm <* symbol "::"))
  tail <- parseVar
  return $ foldr (\h t -> C "Cons" [h, t]) tail headTerms
  <?> "consList"

listTerm :: Parser (Term X)
listTerm =
  try (roundBr consList <|> elemList)
  <?> "listTerm"

termsToList :: [Term X] -> Term X
termsToList = foldr (\h t -> C "Cons" [h, t]) (C "Nil" [])

elemList :: Parser (Term X)
elemList = boxBr (do
  terms <- commaSep parseTerm
  return (termsToList terms))
  <?> "elemList"

parseFresh :: Parser (G X)
parseFresh = do
  symbol "fresh"
  names <- commaSep1 ident
  symbol "in"
  goal <- parseGoal
  return (foldr Fresh goal names)
  <?> "parseFresh"

parseDelay :: Parser (G X)
parseDelay =
      Delay
  <$> (try delay
   *> parseInvocation)
  <?> "parseDelay"

parseInvocation :: Parser (G X)
parseInvocation =
      Invoke
  <$> try ident
  <*> (try parseArguments <|> roundBr (return []))
  <?> "parseInvocation"

parseTerm :: Parser (Term X)
parseTerm =
  try (C <$> try constructorName <*> parseArguments)
  <|> parseSimpleTerm
  <?> "parseTerm"

parseArguments :: Parser [Term X]
parseArguments =
  some (parseSimpleTerm <|> roundBr parseTerm)
  <?> "parseArguments"


parseSimpleTerm :: Parser (Term X)
parseSimpleTerm =
      parseVar
  <|> parseConst
  <|> listTerm
  <?> "parseSimpleTerm"

parseConst :: Parser (Term X)
parseConst =
  C <$> try constructorName <*> return []
  <?> "parseConst"

parseVar :: Parser (Term X )
parseVar =
  V <$> try ident
  <?> "parseVar"

parseSimpleGoal :: Parser (G X)
parseSimpleGoal =
  try parseDelay
  <|> try parseInvocation
  <|> try parseUnification
  <?> "parseSimpleGoal"

parseUnification :: Parser (G X)
parseUnification = try (do
  l <- parseTerm
  symbol "=="
  r <- parseTerm
  return (l :=: r))
  <?> "parseUnification"

parseOp :: Parser (G X)
parseOp =
  unsafeDisj <$> sepBy1 (unsafeConj <$> sepBy1 (parseSimpleGoal <|> roundBr parseGoal) (symbol "&")) (symbol "|")
  <?> "parseOp"

parseGoal :: Parser (G X)
parseGoal = sc
   *> try parseOp
  <|> try parseFresh
  <?> "parseGoal"

parseQuery :: Parser (G X)
parseQuery = do
  symbol "?"
  parseGoal
  <?> "parseQuery"

-- definition
parseDef :: Parser (Def G X)
parseDef = do
  name <- ident
  args <- many ident
  symbol "="
  goal <- parseGoal
  symbol ";"
  return $ Def name args goal
  <?> "parseDef"

-- program
parseProg :: Parser (Program G X)
parseProg = do
  let failureRel = Def "fail" [] (Invoke "fail" [])
  Program <$> ((failureRel :) <$> many parseDef) <*> (postEval [] <$> parseQuery)
  <?> "parseProg"
