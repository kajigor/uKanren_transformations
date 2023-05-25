{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module FunConversion.OCamlPretty where

import qualified Data.Text                 as T
import           FunConversion.Syntax hiding (Error)
import           Prettyprinter
import           Prettyprinter.Render.Text (renderStrict)
import           Util.String

type Error = T.Text
type Prog = Doc T.Text

preamble :: Prog
preamble =
  let gtImport = open <+> pretty' "GT" in
  let ocanrenImport = open <+> pretty' "OCanren" in
  let streamImport = open <+> pretty' "OCanren.Stream" in
  let define name body = letKW <+> name <+> pretty' "=" <+> body in
  let letStar = define (parens (letStarKW)) (pretty' "OCanren.Stream.bind") in
  let retrn = define returnKW (pretty' "OCanren.Stream.single") in
  let mzero = define mzeroKW (pretty' "OCanren.Stream.nil") in
  let mplus = define mplusKW (pretty' "OCanren.Stream.mplus") in
  let msum = define (msumKW <+> pretty' "xs") (pretty' "List.fold_right" <+> mplusKW <+> pretty' "xs" <+> mzeroKW) in
  let guardDef = define (pretty' "guard" <+> pretty' "p") (pretty' "if" <+> pretty' "p" <+> pretty' "then" <+> returnKW <+> pretty' "()" <+> pretty' "else" <+> mzeroKW) in
  let imports = vsep [gtImport, ocanrenImport, streamImport] in
  let definitions = vsep [letStar, retrn, mzero, mplus, msum, guardDef] in
  imports <> line <> line <> definitions

prettyString :: ShowPretty a => a -> String
prettyString x =
  case showPretty x of
    Left err -> T.unpack err
    Right p -> T.unpack . renderStrict . layoutPretty defaultLayoutOptions $ preamble <> line <> p

class ShowPretty a where
  showPretty :: a -> Either Error Prog

instance ShowPretty String where
  showPretty = return . pretty . T.pack

instance ShowPretty Term where
  showPretty (Var v) = showPretty v
  showPretty (Con name args)
    | null name = Left "Constructor name cannot be empty"
    | otherwise = do
        let con = toUpper name
        ts <- mapM showPretty args
        return $ hsep (pretty con : ts)

pretty' :: T.Text -> Doc ann
pretty' = pretty

ignore = letStar (pretty' "_")
equal = pretty' "=="
guard = pretty' "guard"
letKW = pretty' "let"
open = pretty' "open"
letStarKW = pretty' "let*"
returnKW = pretty' "return"
mzeroKW = pretty' "mzero"
mplusKW = pretty' "mplus"
msumKW = pretty' "msum"


letStar var value = letStarKW <+> var <+> pretty' "=" <+> value <+> pretty' "in"

instance ShowPretty Lang where
  showPretty (Call Delayed name args generators) = do
    call <- showPretty (Call NotDelayed name args generators)
    return $ pretty' "delay" <+> pretty' "@@" <+> pretty' "fun" <+> pretty' "()" <+> pretty' "->" <+> call
  showPretty (Call _ name args generators)
    | null name = Left "Relation name cannot be empty"
    | otherwise = do
        args <- mapM showPretty args
        return (pretty name <+> hsep args)
  showPretty (Return []) = do
    return (returnKW <+> pretty' "()")
  showPretty (Return args) = do
      args <- mapM showPretty args
      return (returnKW <+> hsep (map parens args))
  showPretty (Guard x y) = do
    x <- showPretty x
    y <- showPretty y
    return (guard <+> parens (x <+> equal <+> y))
  showPretty (Gen gen) =
    showPretty gen
  showPretty (Match var branch) = do
      var <- showPretty var
      branch <- go branch
      return (matchWith var ([branch, mzeroBranch]))
    where
      go (pattern, branch) = do
        pattrn <- showPretty pattern
        branch <- showPretty branch
        return (pretty' "|" <+> pattrn <+> pretty' "->" <+> branch)
      matchWith var branches =
        pretty' "match" <+> var <+> pretty' "with" <> line <> indent tabSize (vsep branches)
      mzeroBranch =
        pretty' "|" <+> pretty' "_" <+> pretty' "->" <+> mzeroKW
  showPretty (Sum branches) = do
    branches <- mapM showPretty branches
    let parenthesized = map parens branches
    return (pretty' "msum" <> line <> indent tabSize (brackets $ vsep $ punctuate semi parenthesized))
  showPretty (Bind []) =
    Left "Empty body"
  showPretty (Bind binds) = do
      let (prefix, lst) = (init binds, last binds)
      binds <- mapM go prefix
      lst <- noLet lst
      return (vsep binds <+> lst)
    where
      noLet (_, body) = do
        showPretty body
      go (vars, body) = do
        body <- showPretty body
        vars <- mapM showPretty vars
        return $ letStar (parensIfNeeded vars) body
      parensIfNeeded [] = pretty' "_"
      parensIfNeeded [x] = x
      parensIfNeeded xs = parens $ hsep $ punctuate comma xs

instance ShowPretty Def where
  showPretty (Def name args gens body) = do
    name <- showPretty name
    args <- mapM showPretty args
    gens <- mapM showPretty gens
    body <- showPretty body
    return $ name <+> hsep args <+> pretty' "=" <> line <> indent tabSize body

tabSize = 2

instance ShowPretty TypeData where
  showPretty (TypeData constructors) = do
      consts <- mapM go constructors
      return $ pretty' "type" <+> pretty' "term" <+> pretty' "=" <> line <> indent tabSize (vsep consts)
    where
      go (const, n) =
        let constructor = pretty' "|" <+> pretty const in
        if n <= 0
        then return constructor
        else return $ constructor <+> pretty' "of" <+> parensIfNeeded (pretty' "term") n
      parensIfNeeded elem 1 = elem
      parensIfNeeded elem n = parens $ hsep $ punctuate (pretty' "*") (replicate n elem)

instance ShowPretty Program where
  showPretty (Program types defs _) = do
      types <- showPretty types
      defs <- mapM showPretty defs
      return $ types <> line <> line <> go defs
    where
      letRec = letKW <+> pretty' "rec"
      go [] = pretty' ""
      go [x] = letRec <+> x
      go (x:xs) = letRec <+> x <> line <> vsep (map (pretty' "and" <+>) xs)

