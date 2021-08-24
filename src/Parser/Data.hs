{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Data where

import Text.Megaparsec ( Parsec )
import Data.Void ( Void(..) )
import Text.Printf ( printf )

type Parser = Parsec Void String

data ParserType = Irina | Simple

data ParserError a = SyntaxError a
                   | FileNotFound a
                   deriving (Eq, Functor)

instance  {-# OVERLAPS #-} Show (ParserError String) where
  show (SyntaxError err) = printf "Syntax error:\n\t%s" err
  show (FileNotFound path) = printf "File not found:\n\t%s" path

instance Show a => Show (ParserError a) where
  show (SyntaxError err) = printf "Syntax error:\n\t%s" (show err)
  show (FileNotFound path) = printf "File not found:\n\t%s" (show path)
