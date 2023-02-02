{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser.SimplePretty where

import qualified Data.Text                 as T
import           Def
import           Prettyprinter
import           Prettyprinter.Render.Text
import           Program
import           Syntax
import           Util.String

type Error = T.Text
type PrettyProg = Doc T.Text

prettyString :: SimplePretty a => a -> String
prettyString x =
  case toSimplePretty x of
    Left err -> T.unpack err
    Right p -> T.unpack . renderStrict . layoutPretty defaultLayoutOptions $ p

class SimplePretty a where
  toSimplePretty :: a -> Either Error PrettyProg

parenIfCon :: Term a -> Doc ann -> Doc ann
parenIfCon (C _ []) = id
parenIfCon (C _ _) = parens
parenIfCon _ = id

parenIfCon' :: SimplePretty a => Term a -> Either Error (Doc T.Text)
parenIfCon' t = parenIfCon t <$> toSimplePretty t

instance SimplePretty X where
  toSimplePretty :: X -> Either Error PrettyProg
  toSimplePretty v
    | null v = Left "Var name cannot be empty"
    | otherwise = Right $ pretty $ toLower v

instance SimplePretty S where
  toSimplePretty :: S -> Either Error PrettyProg
  toSimplePretty v = Right $ prefixVar $ viaShow v

instance SimplePretty a => SimplePretty (Term a) where
  toSimplePretty :: SimplePretty a => Term a -> Either Error PrettyProg
  toSimplePretty (V v) = toSimplePretty v
  toSimplePretty (C name terms) = prettyCons name terms

prettyCons :: SimplePretty a => Name -> [Term a] -> Either Error (Doc T.Text)
prettyCons name terms
    | null name = Left "Constructor name cannot be empty"
    | otherwise = do
      let con = toUpper name
      ts <- mapM parenIfCon' terms
      return $ hsep (pretty con : ts)

prefixVar :: Doc ann -> Doc ann
prefixVar = (pretty ("v." :: T.Text) <>)

instance SimplePretty a => SimplePretty (G a) where
  toSimplePretty :: G a -> Either Error PrettyProg
  toSimplePretty (x :=: y) = do
    x <- toSimplePretty x
    y <- toSimplePretty y
    return $ x <+> unifyKW <+> y
  toSimplePretty (Invoke name args)
    | null name =  Left "Relation name cannot be empty"
    | otherwise = do
        args <- mapM toSimplePretty args
        return (pretty name <+> hsep args)
  toSimplePretty g@(Disjunction x y xs) = do
    disjuncts <- mapM (\h -> parensIfNeeded g h <$> toSimplePretty h) (x:y:xs)
    return (vsep $ punctuate disjOp disjuncts)
  toSimplePretty g@(Conjunction x y xs) = do
    conjuncts <- mapM (\h -> parensIfNeeded g h <$> toSimplePretty h) (x:y:xs)
    return (hcat $ punctuate conjOp conjuncts)
  toSimplePretty g@(Fresh _ b) = do
    let (names, goal) = collectFreshVars g
    goal <- toSimplePretty goal
    names <- mapM toSimplePretty names
    return $ parensIfNeeded g b (freshKW <+> hcat (punctuate (comma <+> "") names) <+> inKW <+> goal)
  toSimplePretty g@(Delay x) = do
    x' <- toSimplePretty x
    return $ parensIfNeeded g x (delayKW <+> x')

unifyKW :: Doc ann
unifyKW = pretty ("==" :: T.Text)

delayKW :: Doc ann
delayKW = pretty ("Delay" :: T.Text)

freshKW :: Doc ann
freshKW = pretty ("fresh" :: T.Text)

inKW :: Doc ann
inKW = pretty ("in" :: T.Text)

precedence :: G a -> Int
precedence (Invoke _ _) = 9
precedence (_ :=: _) = 9
precedence (Conjunction _ _ _) = 3
precedence (Disjunction _ _ _) = 2
precedence (Fresh _ _) = 1
precedence (Delay _) = 0

parensIfNeeded :: G a -> G a -> Doc ann -> Doc ann
parensIfNeeded curr nested
  | precedence curr > precedence nested = parens
  | otherwise = id

tabSize :: Int
tabSize = 2

spaced :: Doc ann -> Doc ann
spaced = enclose space space

conjOp :: Doc ann
conjOp = spaced $ pretty ("&" :: T.Text)

disjOp :: Doc ann
disjOp = spaced $ pretty ("|" :: T.Text)

unifOp :: Doc ann
unifOp = pretty ("==" :: T.Text)

instance SimplePretty a => SimplePretty (Def G a) where
  toSimplePretty :: SimplePretty a => Def G a -> Either Error PrettyProg
  toSimplePretty (Def name args body)
    | null name = Left "Definition name cannot be empty"
    | otherwise = do
        args <- mapM toSimplePretty args
        body <- toSimplePretty body
        return $ pretty name <+> hsep args <+> equals <> line <> indent tabSize body

instance SimplePretty a => SimplePretty (Program G a) where
  toSimplePretty :: SimplePretty a => Program G a -> Either Error PrettyProg
  toSimplePretty (Program defs goal) = do
    defs <- mapM toSimplePretty defs
    goal <- toSimplePretty goal
    return $ vsep defs <> line <> goal
