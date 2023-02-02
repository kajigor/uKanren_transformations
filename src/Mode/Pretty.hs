{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Mode.Pretty where

import qualified Data.Text                 as T
import           Def
import           Mode.Inst
import           Mode.Syntax
import           Mode.Term
import           Prettyprinter
import           Prettyprinter.Render.Text (renderStrict)
import           Program
import           Util.String

type Error = T.Text
type Prog = Doc T.Text

prettyString :: ShowPretty a => a -> String
prettyString x =
  case showPretty x of
    Left err -> T.unpack err
    Right p -> T.unpack . renderStrict . layoutPretty defaultLayoutOptions $ p

class ShowPretty a where
  showPretty :: a -> Either Error Prog

instance ShowPretty String where
  showPretty = return . pretty

instance ShowPretty Int where
  showPretty = return . viaShow

instance (ShowPretty a, ShowPretty b) => ShowPretty (a, b) where
  showPretty (x, y) = do
    x <- showPretty x
    y <- showPretty y
    return $ x <> brackets y

instance ShowPretty Mode where
  showPretty =
    return . viaShow

instance ShowPretty a => ShowPretty (Var a) where
  showPretty (Var v) = do
    v <- showPretty v
    return $ pretty ("v." :: T.Text) <> v

instance ShowPretty a => ShowPretty (FlatTerm a) where
  showPretty :: ShowPretty a => FlatTerm a -> Either Error Prog
  showPretty (FTCon name terms)
    | null name = Left "Constructor name cannot be empty"
    | otherwise = do
        let con = toUpper name
        ts <- mapM showPretty terms
        return $ hsep (pretty con : ts)
  showPretty (FTVar v) =
    showPretty v

instance ShowPretty a => ShowPretty (Goal a) where
  showPretty (Call name args)
    | null name =  Left "Relation name cannot be empty"
    | otherwise = do
        args <- mapM showPretty args
        return (pretty name <+> hsep args)
  showPretty g@(Disj x y xs) = do
    disjuncts <- mapM (\h -> parensIfNeeded g h <$> showPretty h) (x:y:xs)
    return (vsep $ punctuate disjOp disjuncts)
  showPretty g@(Conj x y xs) = do
    conjuncts <- mapM (\h -> parensIfNeeded g h <$> showPretty h) (x:y:xs)
    return (hcat $ punctuate conjOp conjuncts)
  showPretty (Unif v t) = do
    v <- showPretty v
    t <- showPretty t
    return (v <+> unifOp <+> t)

precedence (Call _ _) = 9
precedence (Unif _ _) = 9
precedence (Conj _ _ _) = 3
precedence (Disj _ _ _) = 2

parensIfNeeded curr nested
  | precedence curr > precedence nested = parens
  | otherwise = id


tabSize = 2

spaced :: Doc ann -> Doc ann
spaced = enclose space space

conjOp = spaced $ pretty ("&&" :: T.Text)
disjOp = spaced $ pretty ("||" :: T.Text)
unifOp = pretty ("==" :: T.Text)

instance ShowPretty a => ShowPretty (Def Goal a) where
  showPretty (Def name args body)
    | null name = Left "Definition name cannot be empty"
    | otherwise = do
        args <- mapM showPretty args
        body <- showPretty body
        return $ pretty name <+> hsep args <+> equals <> line <> indent tabSize body

instance ShowPretty a => ShowPretty (Program Goal a) where
  showPretty (Program defs goal) = do
    defs <- mapM showPretty defs
    goal <- showPretty goal
    return $ vsep defs <> line <> goal
