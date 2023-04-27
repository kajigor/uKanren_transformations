{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module FunConversion.Syntax where
import           Util.String
import qualified Language.Haskell.TH as TH
import Debug.Trace

type Var = String
type Name = String
type Op = String
type Generator = String

data Term = Var Var
          | Con Name [Var] deriving (Show, Eq)

data Delayed = Delayed
             | NotDelayed
             deriving (Show, Eq)

data Lang = Call Delayed Name [Var] [Generator]
          | Return [Term]
          | Sum [Lang]
          | Match Var [(Term, Lang)]
          | Bind [([Var], Lang)]
          | Guard Var Var 
          | Gen Generator deriving (Show, Eq)

data Def = Def Name ([Term], [Generator], Lang) deriving (Show, Eq)


newtype TypeData = TypeData [(String, Int)] deriving (Show, Eq)

data Program = Program { types :: TypeData, defs :: [Def], body :: Maybe Lang } deriving (Show, Eq)

data ProgramDec = ProgramDec { decs :: [TH.Dec], callGoal :: Maybe TH.Exp }

type Error = String

class Quotable a q | a -> q where
  toQuote :: a -> Either Error q

-- TODO: Separate type from function
embedProg :: String -> Either String Program -> [TH.Dec]
embedProg n (Right p@(Program _ _ call)) = case toQuote p of
  Left e -> error e
  Right (ProgramDec decs body) -> callToDec n call body ++ decs
embedProg n e = error $ "Invalid prog for embed: " ++ show e

embedProgSafe :: String -> Program -> Either String [TH.Dec]
embedProgSafe n p@(Program _ _ call) = case toQuote p of
  Left e -> Left e
  Right (ProgramDec decs body) -> Right $ decs ++ callToDec n call body

callToDec :: String -> Maybe Lang -> Maybe TH.Exp -> [TH.Dec]
callToDec n (Just (Call _ _ args gens)) (Just body) = [TH.FunD (TH.mkName n) [TH.Clause (map pvar args ++ map pgen gens) (TH.NormalB body) []]]
callToDec _ _ _ = []

qvar :: Var -> Either Error TH.Exp
qvar v
    | null v = Left "Var name cannot be empty"
    | otherwise = Right $ TH.VarE $ TH.mkName $ toLower v

qgen :: Generator -> Either Error TH.Exp
qgen v
    | null v = Left "Gen name cannot be empty"
    | otherwise = Right $ TH.VarE $ TH.mkName $ toLower v

pgen :: Generator -> TH.Pat 
pgen v = TH.VarP $ TH.mkName $ toLower v

pvar :: Var -> TH.Pat
pvar v = TH.VarP $ TH.mkName $ toLower v

pterm :: Term -> TH.Pat
pterm (Var v) = pvar v
pterm (Con n args) = TH.ConP (TH.mkName n) [] (map pvar args)

instance Quotable Term TH.Exp where
  toQuote (Var v) = qvar v
  toQuote (Con v terms)
    | null v = Left "Constructor name cannot be empty"
    | otherwise = do
      let con = toUpper v
      ts <- mapM qvar terms
      return $ TH.ConE (TH.mkName con) $: ts

instance Quotable Def TH.Dec where
  toQuote (Def name def) = do
    ds <- go def
    return $ TH.FunD (TH.mkName name) [ds]
    where
      go :: ([Term], [Generator], Lang) -> Either Error TH.Clause
      go (args, gens, body) = do
        let p = map pterm args ++ map pgen gens
        b <- toQuote body
        return $ TH.Clause p (TH.NormalB b) []

($:) = foldl TH.AppE

instance Quotable Lang TH.Exp where
  toQuote (Call d name args gens) = do
    let immature = if d == Delayed then TH.AppE (TH.ConE $ TH.mkName "Immature") else id
    let n = toLower name
    xs <- mapM qvar args
    gs <- mapM qgen gens
    return $ immature $ TH.VarE (TH.mkName n) $: (xs ++ gs) -- TODO: Test immature
  toQuote (Gen g) = do
    let n = toLower g
    return $ TH.VarE (TH.mkName n)
  toQuote (Return exprs) = do
    exprs <- mapM ((Just <$>) . toQuote) exprs
    let e = case exprs of
          [] -> TH.TupE []
          [Just e] -> e
          es -> TH.TupE es
    return $ TH.VarE (TH.mkName "return") $: [e]
  toQuote (Sum exprs) = do
    exprs <- mapM toQuote exprs
    return $ TH.VarE (TH.mkName "msum") $: [TH.ListE exprs]
  toQuote (Match v branches) = do
    v' <- qvar v
    bs <- mapM go branches
    let ot = TH.Match TH.WildP (TH.NormalB $ TH.VarE $ TH.mkName "mzero") []
    return $ TH.CaseE v' (bs ++ [ot])
    where
      go :: (Term, Lang) -> Either Error TH.Match
      go (patt, body) = do
        let p = pterm patt
        b <- toQuote body
        return $ TH.Match p (TH.NormalB b) []
  toQuote (Guard a b) = do
    x <- qvar a
    y <- qvar b
    return $ TH.VarE (TH.mkName "guard") $: [TH.InfixE (Just x) (TH.VarE $ TH.mkName "==") (Just y)]
  toQuote (Bind [([], ret)]) = do
    toQuote ret
  toQuote (Bind stmts) = do
    xs <- mapM go stmts
    return $ TH.DoE Nothing xs
    where
      go :: ([Var], Lang) -> Either Error TH.Stmt
      go (vars, Return exprs) | not (null vars) = do
        let vs = map pvar vars
        s <- mapM toQuote exprs
        if length vars == length exprs then
          return $ TH.LetS (zipWith (\n v -> TH.ValD n (TH.NormalB v) []) vs s)
        else
          Left ("Variable mismatch: " ++ show vars ++ " / " ++ show exprs)
      go (vars, stmt) = do
        let vs = map (TH.VarP . TH.mkName . toLower) vars
        s <- toQuote stmt
        return $ (case vs of
          [] -> TH.NoBindS
          [v] -> TH.BindS v
          vs -> TH.BindS (TH.TupP vs)
          ) s

instance Quotable TypeData TH.Dec where

  toQuote (TypeData l) = do
    l <- mapM go l
    return $ TH.DataD [] (TH.mkName "Term") [] Nothing l [TH.DerivClause Nothing [TH.ConT $ TH.mkName "Show", TH.ConT $ TH.mkName "Eq"]]
    where
      go :: (String, Int) -> Either Error TH.Con
      go (n, i) = Right $ TH.NormalC (TH.mkName n) (replicate i (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, TH.ConT $ TH.mkName "Term"))

instance Quotable Program ProgramDec where
  toQuote (Program types defs body) = do
    t <- toQuote types
    d <- mapM toQuote defs
    b <- traverse toQuote body
    return $ ProgramDec (t:d) b