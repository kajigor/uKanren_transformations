{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module FunConversion.OCamlPretty where

import           Data.Char                 (isSpace)
import qualified Data.Text                 as T
import           FunConversion.Syntax      hiding (Error)
import           Prettyprinter
import           Prettyprinter.Render.Text (renderStrict)
import           Util.String

type Error = T.Text
type Prog = Doc T.Text

-- module type Monad_type = sig
--   type 'a t
--   val return : 'a -> 'a t
--   val bind : 'a t -> ('a -> 'b t) -> 'b t
--   val mplus : 'a t -> 'a t -> 'a t
--   val msum : 'a t list -> 'a t
--   val guard : bool -> unit t
--   val make_lazy : (unit -> 'a) -> 'a
-- end

-- module OptionM : Monad_type = struct
--   type 'a t = 'a option
--   let bind x f =
--     match x with
--     | Some x -> f x
--     | None -> None

--   let mplus x y =
--     match x with
--     | Some x -> Some x
--     | None -> y

--   let (let*) = bind
--   let return x = Some x
--   let mzero = None
--   let msum xs = List.fold_right mplus xs mzero
--   let guard p = if p then return () else mzero
--   let make_lazy f = f ()
-- end

-- module StreamM : Monad_type = struct
--   type 'a t = 'a OCanren.Stream.t
--   let (let*) = OCanren.Stream.bind
--   let return = OCanren.Stream.single
--   let mzero = OCanren.Stream.nil
--   let mplus = OCanren.Stream.mplus
--   let msum xs = List.fold_right mplus xs mzero
--   let guard p = if p then return () else mzero
--   let make_lazy f = OCanren.Stream.from_fun f
-- end

preamble :: Prog
preamble =
    let gtImport = open <+> pretty' "GT" in
    let ocanrenImport = open <+> pretty' "OCanren" in
    let streamImport = open <+> pretty' "OCanren.Stream" in

    let letStar = define (parens (letStarKW)) (pretty' "OCanren.Stream.bind") in
    let retrn = define returnKW (pretty' "OCanren.Stream.single") in
    let mzero = define mzeroKW (pretty' "OCanren.Stream.nil") in
    let mplus = define mplusKW (pretty' "OCanren.Stream.mplus") in
    let msum = define (msumKW <+> pretty' "xs") (pretty' "List.fold_right" <+> mplusKW <+> pretty' "xs" <+> mzeroKW) in
    let guardDef = define (pretty' "guard" <+> pretty' "p") (pretty' "if" <+> pretty' "p" <+> pretty' "then" <+> returnKW <+> pretty' "()" <+> pretty' "else" <+> mzeroKW) in
    let delayDef = define (pretty' "make_lazy" <+> pretty' "f") (pretty' "OCanren.Stream.from_fun" <+> pretty' "f") in

    let imports = vsep [gtImport, ocanrenImport, streamImport] in

    let definitions = vsep [letStar, retrn, mzero, mplus, msum, guardDef, delayDef] in

    imports <> line <> line <> definitions

  where
    define name body = letKW <+> name <+> pretty' "=" <+> body


prettyString :: ShowPretty a => a -> String
prettyString x =
  case showPretty x of
    Left err -> T.unpack err
    Right p -> T.unpack . defaultRender $ preamble <> line <> p

defaultRender = renderStrict . layoutPretty defaultLayoutOptions

tuple [] = pretty' "()"
tuple [x] = x
tuple xs = parens $ hsep $ punctuate (pretty' ",") xs

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
        if not $ null args
        then do
          args <- mapM showPretty args
          return $ pretty con <+> tuple args
        else
          return $ pretty con

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

parenthesizeIfSpace :: Doc ann -> Doc ann
parenthesizeIfSpace doc =
    if containsSpace doc
    then parens doc
    else doc
  where
    containsSpace doc =
      let rendered = defaultRender doc in
      T.any isSpace rendered

letStar var value = letStarKW <+> var <+> pretty' "=" <+> value <+> pretty' "in"

instance ShowPretty Lang where
  showPretty (Call Delayed c name args generators) = do
    call <- showPretty (Call NotDelayed c name args generators)
    return $ pretty' "make_lazy" <+> pretty' "@@" <+> pretty' "fun" <+> pretty' "()" <+> pretty' "->" <+> call
  showPretty (Call _ c name args generators)
    | null name = Left "Relation name cannot be empty"
    | otherwise = do
          args <- mapM showPretty args
          generators <- mapM showPretty generators
          return $ convert c $ (pretty name <+> hsep args <+> hsep generators)
        where
          convert NoConversion = id
          convert FromMaybe = (pretty' "optionToStream" <+>)
  showPretty (Return []) = do
    return (returnKW <+> pretty' "()")
  showPretty (Return [x]) = do
    x <- showPretty x
    return (returnKW <+> parenthesizeIfSpace x)
  showPretty (Return args) = do
    args <- mapM showPretty args
    return (returnKW <+> parens (hsep (punctuate comma args)))
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
  showPretty (Def name args gens body isSemidet) = do
    let openStmt = pretty' "let open" <+> pretty' (if isSemidet then "OptionM in" else "StreamM in")
    name <- showPretty name
    args <- mapM showPretty args
    gens <- mapM showPretty gens
    body <- showPretty body
    return $ name <+> hsep args <+> hsep gens <+> pretty' "=" <> line
      <> indent tabSize openStmt <> line
      <> indent tabSize body

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
      parensIfNeeded elem n = parens $ hsep $ punctuate (pretty' " *") (replicate n elem)

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

