{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Data where

import           Data.List          (intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Void          (Void (..))
import           Text.Megaparsec    (Parsec)
import           Text.Printf        (printf)

type Parser = Parsec Void String

data ParserType = Irina | Simple | AnnotatedParser

data ParserError a = SyntaxError a
                   | FileNotFound a
                   | CompoundError (NonEmpty (ParserError a))
                   deriving (Eq, Functor)

instance Semigroup (ParserError a) where
  CompoundError err1 <> CompoundError err2 = CompoundError $ err1 <> err2
  CompoundError errs <> err = CompoundError $ errs <> (err :| [])
  err <> CompoundError (h :| t) = CompoundError $ err :| (h : t)
  err1 <> err2 = CompoundError $ err1 :| [err2]

instance  {-# OVERLAPS #-} Show (ParserError String) where
  show (SyntaxError err) = printf "Syntax error:\n\t%s" err
  show (FileNotFound path) = printf "File not found:\n\t%s" path
  show (CompoundError (h :| t)) =
    printf "Parsing errors:\n\n%s\n%s" (show h) (intercalate "\n" $ map show t :: String )

instance Show a => Show (ParserError a) where
  show (SyntaxError err) = printf "Syntax error:\n\t%s" (show err)
  show (FileNotFound path) = printf "File not found:\n\t%s" (show path)
  show (CompoundError (h :| t)) =
    printf "Parsing errors:\n\n%s\n%s" (show h) (intercalate "\n" $ map show t :: String )
