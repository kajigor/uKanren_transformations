{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
module Parser.AnnotatedParser where

import           Parser.SimpleParser (parseImport, ident, parseGoal, parseQuery, delay, parseArguments, parseUnification, commaSep1, parseTerm)
import           BTA.AnnotatedDef
import           BTA.AnnotationType
import           Eval            (postEval)
import           Parser.Data     (Parser)
import           Parser.Lexer    (boxBr, comma, lIdentifier, lexeme, roundBr, sc, symbol, uIdentifier)
import           BTA.AnnotatedProgram
import           Syntax          (G (..), Term (..), X, unsafeConj, unsafeDisj)
import           Text.Megaparsec (MonadParsec (try), many, sepBy, sepBy1, some, (<?>), (<|>))
import qualified BTA.InvokeAnnotation as Inv
import           Debug.Trace

parseProgramWithImports :: Parser ([String], AnnotatedProgram G X)
parseProgramWithImports = sc *> do
    imports <- many parseImport
    program <- parseProg
    return (imports, program)

parseStatic :: Parser AnnotationType
parseStatic = do
    symbol "static"
    return Static
    <?> "static type"

parseDynamic :: Parser AnnotationType
parseDynamic = do
    symbol "dynamic"
    return Dynamic
    <?> "dynamic type"

parseTypeConstructor :: Parser AnnotationType
parseTypeConstructor = do
    name <- ident
    symbol "("
    types <- many parseType
    symbol ")"
    return $ TypeC name types
    <?> "type constructor"

parseType :: Parser AnnotationType
parseType = 
    parseStatic <|>
    parseDynamic <|>
    parseTypeConstructor 
    <?> "type"

parseTypes :: Parser [AnnotationType]
parseTypes = 
    many parseType 
    <?> "types"

parseArgs :: Int -> Parser [X] 
parseArgs num | num == 0 = do
    return []
parseArgs num = do
    name <- ident
    others <- parseArgs $ num - 1
    return $ name : others


-- definition
parseTypeDef :: Parser (AnnotatedDef G X)
parseTypeDef = do
  symbol "filter"
  types <- roundBr parseTypes
  name <- ident
  args <- parseArgs $ length types
  symbol "="
  goal <- parseGoal
  symbol ";"
  return $ AnnotatedDef name args goal types
  <?> "typed Definition"

-- program
parseProg :: Parser (AnnotatedProgram G X)
parseProg = do
  let failureRel = AnnotatedDef "fail" [] (Invoke "fail" []) []
  AnnotatedProgram <$> ((failureRel :) <$> many parseTypeDef) <*> (postEval [] <$> parseQuery)
  <?> "parseProg"


parseMemo :: Parser Inv.Ann 
parseMemo = do 
    symbol "Memo"
    return Inv.Memo
    <?> "Memo ann"

parseUnfold :: Parser Inv.Ann 
parseUnfold = do 
    symbol "Unfold"
    return Inv.Unfold
    <?> "Unfold ann"

parseAnnType :: Parser Inv.Ann
parseAnnType = 
    parseMemo <|>
    parseUnfold <?>
    "AnnType"

parseAnnUnification :: Parser (Inv.AnnG Term X)
parseAnnUnification = try (do
  l <- parseTerm
  symbol "=="
  r <- parseTerm
  return (l Inv.:=: r))
  <?> "parseUnification"

parseAnnFresh :: Parser (Inv.AnnG Term X)
parseAnnFresh = do
  symbol "fresh"
  names <- commaSep1 ident
  symbol "in"
  goal <- parseAnnGoal
  return (foldr Inv.Fresh goal names)
  <?> "parseAnnFresh"


parseAnnDelay :: Parser (Inv.AnnG Term X)
parseAnnDelay =
      Inv.Delay
  <$> (try delay
   *> parseAnnInvocation)
  <?> "parseAnnDelay"

parseAnnInvocation :: Parser (Inv.AnnG Term X)
parseAnnInvocation = do 
  ann <- parseAnnType
  name <- ident 
  args <- (try parseArguments <|> roundBr (return []))
  return $ Inv.Invoke name args ann 
  <?> "parseInvocation"

parseSimpleAnnGoal :: Parser (Inv.AnnG Term X)
parseSimpleAnnGoal =
  try parseAnnDelay
  <|> try parseAnnInvocation
  <|> try parseAnnUnification
  <?> "parseSimpleAnnGoal"

parseAnnOp :: Parser (Inv.AnnG Term X)
parseAnnOp =
  Inv.unsafeAnnDisj <$> sepBy1 (Inv.unsafeAnnConj <$> sepBy1 (parseSimpleAnnGoal <|> roundBr parseAnnGoal) (symbol "&")) (symbol "|")
  <?> "parseAnnOp"

parseAnnGoal :: Parser (Inv.AnnG Term X) 
parseAnnGoal = sc
   *> try parseAnnOp
  <|> try parseAnnFresh
  <?> "parseAnnGoal"

parseTypeAnnDef :: Parser (AnnotatedDef (Inv.AnnG Term) X) 
parseTypeAnnDef = do 
  symbol "filter"
  types <- (roundBr parseTypes)
  name <- ident
  args <- (parseArgs $ length types)
  symbol "="
  goal <- parseAnnGoal
  symbol ";"
  return $ AnnotatedDef name args goal types
  <?> "typed Definition"

parseAnnQuery :: Parser (Inv.AnnG Term X)
parseAnnQuery = do
  symbol "?"
  parseAnnGoal
  <?> "parseAnnQuery"

parseAnnotatedProg :: Parser (AnnotatedProgram (Inv.AnnG Term) X) 
parseAnnotatedProg = do 
    AnnotatedProgram <$> (many parseTypeAnnDef) <*> parseAnnGoal


parseAnnProgramWithImports :: Parser ([String], AnnotatedProgram (Inv.AnnG Term) X)
parseAnnProgramWithImports = sc *> do
    imports <- many parseImport
    program <- parseAnnotatedProg
    return (imports, program)