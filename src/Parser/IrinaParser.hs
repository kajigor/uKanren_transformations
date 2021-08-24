{-
  This parser was created by Irina Artemeva and since then modified.
  Available at https://github.com/Pluralia/uKanren_translator/blob/58ab786f94f68da1027428706448fbddb980fa72/src/Parser.hs
-}

{-# LANGUAGE DerivingStrategies #-}
module Parser.IrinaParser (parseProg, parseProgramWithImports) where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Eval                           (postEval)
import Text.Megaparsec ( (<|>), many, some, MonadParsec(try) )
import Syntax
    ( G((:=:), Invoke, Fresh),
      Program(..),
      Def(..),
      Term(..),
      X,
      unsafeDisj,
      unsafeConj',
      unsafeDisj' )
import Parser.Data ( Parser )
import Parser.Lexer
    ( sc, symbol, roundBr, angleBr, boxBr, curvyBr, identifier )

parseProgramWithImports :: Parser ([String], Program)
parseProgramWithImports = do
    imports <- many parseImport
    program <- parseProg
    return (imports, program)

parseImport :: Parser String
parseImport = do
  symbol "import"
  ident

reserved :: [String]
reserved = ["trueo", "falso", "zero", "succ", "conde", "import"]

ident :: Parser String
ident = identifier reserved
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
  return $ foldr1 (\term acc -> C "Cons" [term, acc]) (x : xs)


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