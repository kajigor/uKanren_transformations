module Parser.Lexer where

import Parser.Data ( Parser )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad ( void )
import Text.Megaparsec.Char
    ( alphaNumChar, char, letterChar, lowerChar, upperChar, spaceChar )
import Text.Megaparsec ( (<|>), between, many, MonadParsec(try) )
import Text.Printf ( printf )

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

-- brackets
roundBr, angleBr, boxBr, curvyBr :: Parser a -> Parser a
roundBr = between (symbol "(") (symbol ")")
angleBr = between (symbol "<") (symbol ">")
boxBr   = between (symbol "[") (symbol "]")
curvyBr = between (symbol "{") (symbol "}")

identLetters :: Parser Char
identLetters =
  char '_' <|> alphaNumChar <|> char '\''

notReserved :: MonadFail m => [String] -> String -> m String
notReserved reserved x | x `elem` reserved = fail $ printf "%s is reserved" (show x)
notReserved reserved x = return x

lIdentifier :: [String] -> Parser String
lIdentifier reserved =
    (lexeme . try) (p >>= notReserved reserved)
  where
    p = (:) <$> lowerChar <*> many identLetters

uIdentifier :: [String] -> Parser String
uIdentifier reserved =
    (lexeme . try) (p >>= notReserved reserved)
  where
    p = (:) <$> upperChar <*> many identLetters

identifier :: [String] -> Parser String
identifier reserved =
    (lexeme . try) (p >>= notReserved reserved)
  where
    p = (:) <$> letterChar <*> many identLetters

comma :: Parser String
comma = symbol ","
