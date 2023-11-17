{-# LANGUAGE FunctionalDependencies #-}

module FunConversion.Updated.Syntax where
import           Util.String
import qualified Language.Haskell.TH as TH

import qualified FunConversion.DetMode as D
import FunConversion.Updated.Utils
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)))

-- MatchVar currently unused
data Var = NormalVar Int | MatchVar Int deriving (Show, Eq, Ord)
type Name = String
type Generator = (D.DefIdentifier, Var)

nameVar :: Var -> Name
nameVar (NormalVar i) = 'x' : show i
nameVar (MatchVar  i) = 'y' : show i

nameDef :: D.DefIdentifier -> Name
nameDef (D.DId (name, modes)) = name ++ nameModes modes
  where
    nameModes [] = ""
    nameModes (In:ms) = 'I' : nameModes ms
    nameModes (Out:ms) = 'O' : nameModes ms
  
nameGen :: Generator -> Name
nameGen (d, v) = nameDef d ++ "_" ++ nameVar v

data Term = Var Var
          | Con Name [Var] deriving (Show, Eq)

data Delayed = Delayed
             | NotDelayed
             deriving (Show, Eq)

data MaybeConversion = FromMaybe
                     | NoConversion
                     deriving (Show, Eq)
data Lang = Empty
          | Sum (NonEmpty Lang)
          | Bind (NonEmpty Lang) [Var]
          | Assn Term Term
          | Guard Var Term
          | Gen Generator Term
          | Call Delayed MaybeConversion D.DefIdentifier [Var] [Generator] [Var]
          deriving (Show, Eq)

data Def = Def { name :: D.DefIdentifier, args :: [Var], generators :: [Generator], body :: Lang } deriving (Show, Eq)

newtype TypeData = TypeData [(String, Int)] deriving (Show, Eq)

data Program = Program { types :: TypeData, defs :: [Def], goal :: Maybe D.DefIdentifier } deriving (Show, Eq)

data ProgramDec = ProgramDec { decs :: [TH.Dec], callGoal :: Maybe TH.Exp }

type Error = String

outVars :: Lang -> [Term]
outVars Empty = []
outVars (Call _ _ _ _ _ ret) = Var <$> ret
outVars (Assn _ ret) = [ret]
outVars (Sum (x :| _)) = outVars x
outVars (Bind xs ret) = Var <$> ret
outVars (Guard _ _) = []
outVars (Gen _ ret) = [ret]

class Quotable a q | a -> q where
  toQuote :: a -> Either Error q

-- TODO: Separate type from function
embedProg :: String -> Either Error Program -> [TH.Dec]
embedProg n p = case p >>= embedProgSafe n of
  Left e -> error e
  Right p -> p

embedProgSafe :: String -> Program -> Either Error [TH.Dec]
embedProgSafe n p = do
  (ProgramDec decs body) <- toQuote p
  call <- callToDec n body
  return $ decs ++ call

callToDec :: String -> Maybe TH.Exp -> Either Error [TH.Dec]
callToDec n (Just body) = do
  return [TH.FunD (TH.mkName n) [TH.Clause [] (TH.NormalB body) []]]
callToDec _ _ = return []

qname :: Name -> Either Error TH.Exp
qname n
      | null n = Left "Name cannot be empty"
      | otherwise = Right $ TH.VarE $ TH.mkName n

pname :: Name -> Either Error TH.Pat
pname n 
     | null n = Left "Var name cannot be empty"
     | otherwise = Right $ TH.VarP $ TH.mkName n

qvar :: Var -> Either Error TH.Exp
qvar = qname . nameVar

pvar :: Var -> Either Error TH.Pat
pvar = pname . nameVar

qgen :: Generator -> Either Error TH.Exp
qgen = qname . nameGen

pgen :: Generator -> Either Error TH.Pat
pgen = pname . nameGen

qdef :: D.DefIdentifier -> Either Error TH.Exp
qdef = qname . nameDef

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
    return $ TH.FunD (TH.mkName $ nameDef (name d)) [ds]
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
  toQuote (Call d conv name args gens _) = do
    let immature = if d == Delayed then TH.AppE (TH.ConE $ TH.mkName "Immature") else id
    let conversion = if conv == FromMaybe then TH.AppE (TH.VarE $ TH.mkName "maybeToStream") else id
    n <- qdef name
    xs <- mapM qvar args
    gs <- mapM qgen gens
    return $ immature $ conversion $ n $: (xs ++ gs) -- TODO: Test immature
  toQuote (Assn expr _) = do
    fn <- qname "return"
    e <- toQuote expr
    return $ fn $: [e]
  toQuote (Guard a b) = do
    fn <- qname "guard"
    eq <- qname "=="
    x <- qvar a
    y <- toQuote b
    return $ fn $: [TH.InfixE (Just x) eq (Just y)]
  toQuote (Gen g _) = qgen g
  toQuote (Sum exprs) = do
    fn <- qname "msum"
    exprs <- mapM toQuote exprs
    return $ fn $: [TH.ListE $ NE.toList exprs]
  toQuote (Bind stmts rets) = do
    xs <- mapM go stmts
    rs <- quoteRets
    return $ TH.DoE Nothing (NE.toList xs ++ [TH.NoBindS rs])
    where

      quoteRets = do
        fn <- qname "return"
        exprs <- mapM qvar rets
        let e = case exprs of
              [e] -> e
              es -> TH.TupE (Just <$> es)
        return $ fn $: [e]

      go :: Lang -> Either Error TH.Stmt
      -- go (Assn exprs vars) | not (null vars) = do
      --   vs <- mapM pterm vars
      --   s <- mapM toQuote exprs
      --   lguard ("Variable mismatch: " ++ show vars ++ " / " ++ show exprs) $ length vars == length exprs
      --   return $ TH.LetS (zipWith (\n v -> TH.ValD n (TH.NormalB v) []) vs s)
      go stmt = do
        let vars = outVars stmt
        vs <- mapM pterm vars
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
    b <- traverse qdef body
    return $ ProgramDec (t:d) b