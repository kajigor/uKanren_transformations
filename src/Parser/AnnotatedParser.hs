{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
module Parser.AnnotatedParser where

import           Parser.SimpleParser (parseImport, ident, parseGoal, parseQuery)
import           AnnotatedDef
import           AnnotationType
import           Eval            (postEval)
import           Parser.Data     (Parser)
import           Parser.Lexer    (boxBr, comma, lIdentifier, lexeme, roundBr, sc, symbol, uIdentifier)
import           AnnotatedProgram
import           Syntax          (G (..), Term (..), X, unsafeConj, unsafeDisj)
import           Text.Megaparsec (MonadParsec (try), many, sepBy, sepBy1, some, (<?>), (<|>))

parseProgramWithImports :: Parser ([String], AnnotatedProgram G X)
parseProgramWithImports = sc *> do
    imports <- many parseImport
    program <- parseProg
    return (imports, program)

parseStatic :: Parser AnnotationType
parseStatic = do
    symbol "static"
    return $ Static
    <?> "parseStatic"

parseDynamic :: Parser AnnotationType
parseDynamic = do
    symbol "dynamic"
    return $ Dynamic
    <?> "parseDynamic"

parseTypeConstructor :: Parser AnnotationType
parseTypeConstructor = do
    name <- ident
    symbol "("
    types <- many parseType
    symbol ")"
    return $ TypeC name types
    <?> "parseTypeConstructor"

parseType :: Parser AnnotationType
parseType = 
    parseStatic <|>
    parseDynamic <|>
    parseTypeConstructor 
    <?> "parseType"

parseTypes :: Parser [AnnotationType]
parseTypes = 
    many parseType 
    <?> "parseTypes"

parseArgs :: Int -> Parser [X] 
parseArgs num | num == 0 = do
    return []
parseArgs num = do
    name <- ident
    others <- (parseArgs $ num - 1)
    return $ name : others


-- definition
parseTypeDef :: Parser (AnnotatedDef G X)
parseTypeDef = do
  symbol "filter"
  types <- (roundBr parseTypes)
  name <- ident
  args <- (parseArgs $ length types)
  symbol "="
  goal <- parseGoal
  symbol ";"
  return $ AnnotatedDef name args goal types
  <?> "parseTypeDef"

-- program
parseProg :: Parser (AnnotatedProgram G X)
parseProg = do
  let failureRel = AnnotatedDef "fail" [] (Invoke "fail" []) []
  AnnotatedProgram <$> ((failureRel :) <$> many parseTypeDef) <*> (postEval [] <$> parseQuery)
  <?> "parseProg"


