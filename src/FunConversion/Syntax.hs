{-# LANGUAGE FunctionalDependencies #-}

module FunConversion.Syntax where
import           Util.String
import qualified Language.Haskell.TH as TH

type Var = String
type Name = String
type Generator = String

data Term = Var Var
          | Con Name [Var] deriving (Show, Eq)

data Delayed = Delayed
             | NotDelayed
             deriving (Show, Eq)

data Lang = Empty
          | Call Delayed Name [Var] [Generator]
          | Return [Term]
          | Sum [Lang]
          | Match Var [(Term, Lang)]
          | Bind [([Var], Lang)]
          | Guard Var Term
          | Gen Generator deriving (Show, Eq)

data Def = Def { name :: Name, args :: [Var], generators :: [Generator], body :: Lang } deriving (Show, Eq)

newtype TypeData = TypeData [(String, Int)] deriving (Show, Eq)

data Program = Program { types :: TypeData, defs :: [Def], goal :: Maybe Lang } deriving (Show, Eq)

data ProgramDec = ProgramDec { decs :: [TH.Dec], callGoal :: Maybe TH.Exp }

type Error = String

class Quotable a q | a -> q where
  toQuote :: a -> Either Error q

-- TODO: Separate type from function
embedProg :: String -> Either Error Program -> [TH.Dec]
embedProg n p = case p >>= embedProgSafe n of
  Left e -> error e
  Right p -> p

embedProgSafe :: String -> Program -> Either Error [TH.Dec]
embedProgSafe n p@(Program _ _ call) = do
  (ProgramDec decs body) <- toQuote p
  call <- callToDec n call body
  return $ decs ++ call

callToDec :: String -> Maybe Lang -> Maybe TH.Exp -> Either Error [TH.Dec]
callToDec n (Just (Call _ _ args gens)) (Just body) = do
  args <- mapM pvar args
  gens <- mapM pgen gens
  return [TH.FunD (TH.mkName n) [TH.Clause (args ++ gens) (TH.NormalB body) []]]
callToDec _ _ _ = return []

qvar :: Var -> Either Error TH.Exp
qvar v
    | null v = Left "Var name cannot be empty"
    | otherwise = Right $ TH.VarE $ TH.mkName v

pvar :: Var -> Either Error TH.Pat
pvar v 
    | null v = Left "Var name cannot be empty"
    | otherwise = Right $ TH.VarP $ TH.mkName v

qgen :: Generator -> Either Error TH.Exp
qgen = qvar

pgen :: Generator -> Either Error TH.Pat
pgen = pvar

qname :: Name -> Either Error TH.Exp
qname = qvar

pterm :: Term -> Either Error TH.Pat
pterm (Var v) = pvar v
pterm (Con n args) = do
  args <- mapM pvar args
  return $ TH.ConP (TH.mkName n) [] args

instance Quotable Term TH.Exp where
  toQuote (Var v) = qvar v
  toQuote (Con v terms)
    | null v = Left "Constructor name cannot be empty"
    | otherwise = do
      let con = toUpper v
      ts <- mapM qvar terms
      return $ TH.ConE (TH.mkName con) $: ts

instance Quotable Def TH.Dec where
  toQuote d = do
    ds <- go d
    return $ TH.FunD (TH.mkName (name d)) [ds]
    where
      go :: Def -> Either Error TH.Clause
      go (Def _ args gens body) = do
        args <- mapM pvar args
        gens <- mapM pgen gens
        b <- toQuote body
        return $ TH.Clause (args ++ gens) (TH.NormalB b) []

($:) :: TH.Exp -> [TH.Exp] -> TH.Exp
($:) = foldl TH.AppE

instance Quotable Lang TH.Exp where
  toQuote Empty = qname "mzero"
  toQuote (Call d name args gens) = do
    let immature = if d == Delayed then TH.AppE (TH.ConE $ TH.mkName "Immature") else id
    n <- qname name
    xs <- mapM qvar args
    gs <- mapM qgen gens
    return $ immature $ n $: (xs ++ gs) -- TODO: Test immature
  toQuote (Gen g) = qgen g
  toQuote (Return exprs) = do
    fn <- qname "return"
    exprs <- mapM toQuote exprs
    let e = case exprs of
          [e] -> e
          es -> TH.TupE (Just <$> es)
    return $ fn $: [e]
  toQuote (Sum exprs) = do
    fn <- qname "msum"
    exprs <- mapM toQuote exprs
    return $ fn $: [TH.ListE exprs]
  toQuote (Match v branches) = do
    fn <- qname "mzero"
    v' <- qvar v
    bs <- mapM go branches
    let ot = TH.Match TH.WildP (TH.NormalB fn) []
    return $ TH.CaseE v' (bs ++ [ot])
    where
      go :: (Term, Lang) -> Either Error TH.Match
      go (patt, body) = do
        p <- pterm patt
        b <- toQuote body
        return $ TH.Match p (TH.NormalB b) []
  toQuote (Guard a b) = do
    fn <- qname "guard"
    eq <- qname "=="
    x <- qvar a
    y <- toQuote b
    return $ fn $: [TH.InfixE (Just x) eq (Just y)]
  toQuote (Bind [([], ret)]) = do
    toQuote ret
  toQuote (Bind stmts) = do
    xs <- mapM go stmts
    return $ TH.DoE Nothing xs
    where
      go :: ([Var], Lang) -> Either Error TH.Stmt
      go (vars, Return exprs) | not (null vars) = do
        vs <- mapM pvar vars
        s <- mapM toQuote exprs
        if length vars == length exprs then
          return $ TH.LetS (zipWith (\n v -> TH.ValD n (TH.NormalB v) []) vs s)
        else
          Left ("Variable mismatch: " ++ show vars ++ " / " ++ show exprs)
      go (vars, stmt) = do
        vs <- mapM pvar vars
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