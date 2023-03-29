{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module FunConversion.Syntax where

import qualified Data.Text     as T
import           Prettyprinter
import           Util.String
import qualified Language.Haskell.TH as TH

type Var = String
type Name = String
type Op = String

data Term = Var Var
          | Con Name [Term] deriving (Show, Eq)

data Expr = T Term
          | BinOp Op Expr Expr deriving (Show, Eq)

data Delayed = Delayed
             | NotDelayed
             deriving (Show, Eq)

data Lang = Empty
          | Let Var Expr
          | Call Delayed Name [Term]
          | Return [Expr]
          | Mplus [Lang]
          | Match Term [(Term, Lang)]
          | Bind [([Var], Lang)]
          | If Expr Lang Lang deriving (Show, Eq)

data Def = Def Name ([Term], Lang) deriving (Show, Eq)


newtype TypeData = TypeData [(String, Int)] deriving (Show, Eq)

data Program = Program { types :: TypeData, defs :: [Def], body :: Maybe Lang } deriving (Show, Eq)

data ProgramDec = ProgramDec { decs :: [TH.Dec], callGoal :: Maybe TH.Exp }

addXZ :: Def
addXZ =
  Def "addXZ" ( [Var "x", Var "z"]
    , Match (Var "x")
      [
        (Con "O" [], Return [T $ Var "z"])
      , (Con "S" [Var "x'"],
          Match (Var "z")
          [ (Con "O" [], Empty)
          , (Con "S" [Var "z'"], Call NotDelayed "addXZ" [Var "x'", Var "z'"])
          ]
        )
      ]
    )


addYZ :: Def
addYZ =
  Def "addYZ" ( [Var "y", Var "z"]
    , If (BinOp "==" (T $ Var "y") (T $ Var "z"))
         (Return [T $ Con "O" []])
         (Match (Var "z")
           [ (Con "S" [Var "z'"]
             , Bind
                [ (["x"], Call NotDelayed "addYZ" [Var "y", Var "z'"])
                , ([], Return [T $ Con "S" [Var "x"]])
                ]
             )
           , (Con "O" []
             , Empty
             )
           ]

         )
    )

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

class Quotable a q | a -> q where
  toQuote :: a -> Either Error q

-- TODO: Separate type from function
embedProg :: String -> Either String Program -> [TH.Dec]
embedProg n (Right p@(Program _ _ call)) = case toQuote p of
  Left e -> error (T.unpack e)
  Right (ProgramDec decs body) -> callToDec n call body ++ decs
embedProg n e = error $ "Invalid prog for embed: " ++ show e

embedProgSafe :: String -> Program -> Either String [TH.Dec]
embedProgSafe n p@(Program _ _ call) = case toQuote p of
  Left e -> Left (T.unpack e)
  Right (ProgramDec decs body) -> Right $ decs ++ callToDec n call body

callToDec :: String -> Maybe Lang -> Maybe TH.Exp -> [TH.Dec]
callToDec n (Just (Call _ _ args)) (Just body) = [TH.FunD (TH.mkName n) [TH.Clause (map toPattern args) (TH.NormalB body) []]]
callToDec _ _ _ = []

parenIfCon :: Term -> Doc ann -> Doc ann
parenIfCon (Con _ []) = id
parenIfCon (Con _ _) = parens
parenIfCon _ = id

parenIfCon' :: Term -> Either Error (Doc T.Text)
parenIfCon' t = parenIfCon t <$> toHaskell t

instance Haskell Term where
  toHaskell :: Term -> Either Error HaskellProg
  toHaskell (Var v)
    | null v = Left "Var name cannot be empty"
    | otherwise = Right $ pretty $ toLower v
  toHaskell (Con v terms)
    | null v = Left "Constructor name cannot be empty"
    | otherwise = do
      let con = toUpper v
      ts <- mapM parenIfCon' terms
      return $ hsep (pretty con : ts)

instance Quotable Term TH.Exp where
  toQuote (Var v)
    | null v = Left "Var name cannot be empty"
    | otherwise = Right $ TH.VarE $ TH.mkName $ toLower v
  toQuote (Con v terms)
    | null v = Left "Constructor name cannot be empty"
    | otherwise = do
      let con = toUpper v
      ts <- mapM toQuote terms
      return $ TH.ConE (TH.mkName con) $: ts

tabSize :: Int
tabSize = 2

instance Haskell Def where
  toHaskell :: Def -> Either Error HaskellProg
  toHaskell (Def name def) = do
      ds <- go def
      return $ pretty name <+> ds
    where
      go (args, body) = do
        as <- mapM parenIfCon' args
        b <- toHaskell body
        return (hsep as <+> equals <> line <> indent tabSize b)

toPattern :: Term -> TH.Pat
toPattern (Var n) = TH.VarP $ TH.mkName n
toPattern (Con n args) = TH.ConP (TH.mkName n) [] (map toPattern args)

instance Quotable Def TH.Dec where
  toQuote (Def name def) = do
    ds <- go def
    return $ TH.FunD (TH.mkName name) [ds]
    where
      go :: ([Term], Lang) -> Either Error TH.Clause
      go (args, body) = do
        let p = map toPattern args
        b <- toQuote body
        return $ TH.Clause p (TH.NormalB b) []

emptyKW :: Doc ann
emptyKW = pretty ("mempty" :: T.Text)

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

guardKW :: Doc ann
guardKW = pretty ("guard" :: T.Text)

otherwiseKW :: Doc ann
otherwiseKW = pretty ("_" :: T.Text)

unitKW :: Doc ann
unitKW = pretty ("()" :: T.Text)

dataKW :: Doc ann
dataKW = pretty ("data" :: T.Text)

termKW :: Doc ann
termKW = pretty ("Term" :: T.Text)

barKW :: Doc ann
barKW = pretty ("| " :: T.Text)

derivesKW :: Doc ann
derivesKW = pretty ("deriving (Show, Eq)" :: T.Text)

immatureFmapKW :: Doc ann
immatureFmapKW = pretty ("Immature <$> " :: T.Text)

tupled' :: [Doc ann] -> Doc ann
tupled' [] = unitKW
tupled' [x] = x
tupled' xs = tupled xs

($:) = foldl TH.AppE

instance Haskell Lang where
  toHaskell :: Lang -> Either Error HaskellProg
  toHaskell Empty = return emptyKW
  toHaskell (Let var e) = do
    let v = pretty $ toLower var
    e <- toHaskell e
    return (letKW <+> v <+> equals <+> e)
  toHaskell (Call d name args) = do
    let immature = if d == Delayed then immatureFmapKW else ""
    let n = pretty $ toLower name
    xs <- mapM (\t -> parenIfCon t <$> toHaskell t) args
    return (immature <> n <+> hsep xs) -- TODO: Test immature
  toHaskell (Return exprs) = do
    exprs <- mapM toHaskell exprs
    return (returnKW <+> tupled exprs)
  toHaskell (Mplus args) = do
    as <- mapM toHaskell args
    return (vsep (punctuate (line <> mplusKW) as))
  toHaskell (Match term branches) = do
      t <- toHaskell term
      bs <- mapM go branches
      let ot = otherwiseKW <+> arrow <+> emptyKW
      return (caseKW <+> t <+> ofKW <> line <> indent tabSize (vsep bs) <> line <> indent tabSize ot)
    where
      go (patt, body) = do
        p <- toHaskell patt
        b <- toHaskell body
        return (p <+> arrow <> line <> indent tabSize b)
  toHaskell (If cond (Return []) Empty) = do
    cond <- toHaskell cond
    return (guardKW <+> cond)
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
      go (vars, Return exprs) = do
        let vs = map (pretty . toLower) vars
        s <- mapM toHaskell exprs
        if null vs
        then
          return (returnKW <+> tupled' s)
        else
          return (letKW <+> tupled' vs <+> equals <+> tupled' s)
      go (vars, stmt) = do
        let vs = map (pretty . toLower) vars
        s <- toHaskell stmt
        if null vs
        then
          return s
        else
          return (tupled' vs <+> backArrow <+> s)

instance Quotable Lang TH.Exp where

  toQuote Empty = return $ TH.VarE $ TH.mkName "mempty"
  toQuote (Let var e) = Left "unimplemented"
  toQuote (Call d name args) = do
    let immature = if d == Delayed then TH.AppE (TH.ConE $ TH.mkName "Immature") else id
    let n = toLower name
    xs <- mapM toQuote args
    return $ immature $ TH.VarE (TH.mkName n) $: xs -- TODO: Test immature
  toQuote (Return exprs) = do
    exprs <- mapM ((Just <$>) . toQuote) exprs
    let e = (case exprs of
          [] -> TH.TupE []
          [Just e] -> e
          es -> TH.TupE es
          )
    return $ TH.VarE (TH.mkName "return") $: [e]
  toQuote (Mplus exprs) = do
    exprs <- mapM toQuote exprs
    return $ TH.VarE (TH.mkName "msum") $: [TH.ListE exprs]
  toQuote (Match term branches) = do
    t <- toQuote term
    bs <- mapM go branches
    let ot = TH.Match TH.WildP (TH.NormalB $ TH.VarE $ TH.mkName "mzero") []
    return $ TH.CaseE t (bs ++ [ot])
    where
      go :: (Term, Lang) -> Either Error TH.Match
      go (patt, body) = do
        let p = toPattern patt
        b <- toQuote body
        return $ TH.Match p (TH.NormalB b) []
  toQuote (If cond (Return []) Empty) = do
    cond <- toQuote cond
    return $ TH.VarE (TH.mkName "guard") $: [cond]
  toQuote (If cond thn els) = do
    cond <- toQuote cond
    thn <- toQuote thn
    els <- toQuote els
    return $ TH.CondE cond thn els
  toQuote (Bind stmts) = do
    xs <- mapM go stmts
    return $ TH.DoE Nothing xs
    where
      go :: ([Var], Lang) -> Either Error TH.Stmt
      go (vars, Return exprs) | not (null vars) = do
        let vs = map (TH.VarP . TH.mkName . toLower) vars
        s <- mapM toQuote exprs
        if length vars == length exprs then
          return $ TH.LetS (zipWith (\n v -> TH.ValD n (TH.NormalB v) []) vs s)
        else
          Left (T.pack $ "Variable mismatch: " ++ show vars ++ " / " ++ show exprs)
      go (vars, stmt) = do
        let vs = map (TH.VarP . TH.mkName . toLower) vars
        s <- toQuote stmt
        return $ (case vs of
          [] -> TH.NoBindS
          [v] -> TH.BindS v
          vs -> TH.BindS (TH.TupP vs)
          ) s

instance Haskell Expr where
  toHaskell (T t) = parenIfCon' t
  toHaskell (BinOp op x y) = do
    x <- toHaskell x
    y <- toHaskell y
    return $ parens (x <+> pretty op <+> y)

instance Quotable Expr TH.Exp where
  toQuote (T t) = toQuote t
  toQuote (BinOp op x y) = do
    x <- toQuote x
    y <- toQuote y
    return $ TH.InfixE (Just x) (TH.VarE $ TH.mkName op) (Just y)

instance Haskell TypeData where
  toHaskell (TypeData l) = do
    l <- mapM go l
    return $ dataKW <+> termKW <+> equals <+> encloseSep "" "" barKW l <+> derivesKW
    where
      go :: (String, Int) -> Either Error HaskellProg
      go (n, i) = return $ pretty n <+> hsep (replicate i termKW)

instance Quotable TypeData TH.Dec where

  toQuote (TypeData l) = do
    l <- mapM go l
    return $ TH.DataD [] (TH.mkName "Term") [] Nothing l [TH.DerivClause Nothing [TH.ConT $ TH.mkName "Show", TH.ConT $ TH.mkName "Eq"]]
    where
      go :: (String, Int) -> Either Error TH.Con
      go (n, i) = Right $ TH.NormalC (TH.mkName n) (replicate i (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, TH.ConT $ TH.mkName "Term"))


instance Haskell Program where
  toHaskell (Program types defs body) = do
    t <- toHaskell types
    d <- mapM toHaskell defs
    b <- maybe (return "") toHaskell body
    return $ t <> line <> vsep d <> line <> b

instance Quotable Program ProgramDec where
  toQuote (Program types defs body) = do
    t <- toQuote types
    d <- mapM toQuote defs
    b <- traverse toQuote body
    return $ ProgramDec (t:d) b