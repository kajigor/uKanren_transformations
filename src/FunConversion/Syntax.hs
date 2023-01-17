{-# LANGUAGE OverloadedStrings #-}

module FunConversion.Syntax where

import Prettyprinter

import qualified Data.Text as T
import qualified Data.Char as C

type Var = String
type Name = String
type Op = String

data Term = Var Var
          | Con Name [Term]

data Expr = T Term
          | BinOp Op Expr Expr

data Lang = Empty
          | Let Var Expr
          | Call Name [Term]
          | Return [Expr]
          | Mplus [Lang]
          | Match Term [(Term, Lang)]
          | Bind [([Var], Lang)]
          | If Expr Lang Lang

data Def = Def Name [([Term], Lang)]

addXZ =
  Def "addXZ" [
    ( [Var "x", Var "z"]
    , Match (Var "x")
      [
        (Con "O" [], Return [T $ Var "z"])
      , (Con "S" [Var "x'"],
          Match (Var "z")
          [ (Con "O" [], Empty)
          , (Con "S" [Var "z'"], Call "addXZ" [Var "x'", Var "z'"])
          ]
        )
      ]
    )
  ]

addYZ =
  Def "addYZ" [
    ( [Var "y", Var "z"]
    , If (BinOp "==" (T $ Var "y") (T $ Var "z"))
         (Return [T $ Con "O" []])
         (Match (Var "z")
           [ (Con "S" [Var "z'"]
             , Bind
                [ (["x"], Call "addYZ" [Var "y", Var "z'"])
                , ([], Return [T $ Con "S" [Var "x"]])
                ]
             )
           , (Con "O" []
             , Empty
             )
           ]

         )
    )
  ]

-- addYZ y z =
--   if y == z
--   then return O
--   else
--     case z of
--       S z' -> do
--         x <- addYZ y z'
--         return (S x)
--       O -> Empty

type Error = T.Text
type HaskellProg = Doc T.Text

class Haskell a where
  toHaskell :: a -> Either Error HaskellProg

modifyFirstLetter _ "" = ""
modifyFirstLetter f x = f (head x) : tail x

toLower = modifyFirstLetter C.toLower

toUpper = modifyFirstLetter C.toUpper

parenIfCon (Con _ []) = id
parenIfCon (Con _ _) = parens
parenIfCon _ = id

parenIfCon' t = parenIfCon t <$> toHaskell t

instance Haskell Term where
  toHaskell (Var v)
    | null v = Left "Var name cannot be empty"
    | otherwise = Right $ pretty $ toLower v
  toHaskell (Con v terms)
    | null v = Left "Constructor name cannot be empty"
    | otherwise = do
      let con = toUpper v
      ts <- mapM parenIfCon' terms
      return $ hsep (pretty con : ts)

tabSize = 2

instance Haskell Def where
  toHaskell (Def name defs) = do
      ds <- mapM go defs
      return $ vsep (map (pretty name <+>) ds)
    where
      go (args, body) = do
        as <- mapM parenIfCon' args
        b <- toHaskell body
        return (hsep as <+> equals <> line <> indent tabSize b)

emptyKW :: Doc ann
emptyKW = pretty ("Empty" :: T.Text)

letKW :: Doc ann
letKW = pretty ("let" :: T.Text)

returnKW :: Doc ann
returnKW = pretty ("return" :: T.Text)

mplusKW :: Doc ann
mplusKW = pretty ("`mplus`" :: T.Text)

caseKW :: Doc ann
caseKW = pretty ("case" :: T.Text)

ofKW :: Doc ann
ofKW = pretty ("of" :: T.Text)

arrow :: Doc ann
arrow = pretty ("->" :: T.Text)

backArrow :: Doc ann
backArrow = pretty ("<-" :: T.Text)

ifKW :: Doc ann
ifKW = pretty ("if" :: T.Text)

thenKW :: Doc ann
thenKW = pretty ("then" :: T.Text)

elseKW :: Doc ann
elseKW = pretty ("else" :: T.Text)

doKW :: Doc ann
doKW = pretty ("do" :: T.Text)

tupled' :: [Doc ann] -> Doc ann
tupled' [] = emptyDoc
tupled' [x] = x
tupled' xs = tupled xs

instance Haskell Lang where
  toHaskell Empty = return emptyKW
  toHaskell (Let var e) = do
    let v = pretty $ toLower var
    e <- toHaskell e
    return (letKW <+> v <+> equals <+> e)
  toHaskell (Call name args) = do
    let n = pretty $ toLower name
    xs <- mapM (\t -> parenIfCon t <$> toHaskell t) args
    return (n <+> hsep xs)
  toHaskell (Return exprs) = do
    exprs <- mapM toHaskell exprs
    return (returnKW <+> tupled exprs)
  toHaskell (Mplus args) = do
    as <- mapM toHaskell args
    return (vsep (punctuate mplusKW as))
  toHaskell (Match term branches) = do
      t <- toHaskell term
      bs <- mapM go branches
      return (caseKW <+> t <+> ofKW <> line <> indent tabSize (vsep bs))
    where
      go (patt, body) = do
        p <- toHaskell patt
        b <- toHaskell body
        return (p <+> arrow <> line <> indent tabSize b)
  toHaskell (If cond thn els) = do
    cond <- toHaskell cond
    thn <- toHaskell thn
    els <- toHaskell els
    return (ifKW <+> cond <> line <>
            thenKW <> line <> indent tabSize thn <> line <>
            elseKW <> line <> indent tabSize els)
  toHaskell (Bind stmts) = do
      xs <- mapM go stmts
      return (doKW <> line <> indent tabSize (vsep xs))
    where
      go :: ([Var], Lang) -> Either Error HaskellProg
      go (vars, stmt) = do
        let vs = map (pretty . toLower) vars
        s <- toHaskell stmt
        if null vs
        then
          return s
        else
          return (tupled' vs <+> backArrow <+> s)

instance Haskell Expr where
  toHaskell (T t) = parenIfCon' t
  toHaskell (BinOp op x y) = do
    x <- toHaskell x
    y <- toHaskell y
    return $ parens (x <+> pretty op <+> y)

