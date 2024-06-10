{-# LANGUAGE FunctionalDependencies #-}
module FunConversion.DetCheck where

import FunConversion.Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace
import Data.List (init, last)
import qualified Language.Haskell.TH as TH

data DetType = DetFail | SemiDet | Det | NonDet deriving (Show, Eq)
type DetState = Map.Map String DetType

detConj DetFail _ = DetFail
detConj _ DetFail = DetFail
detConj NonDet _  = NonDet
detConj _ NonDet  = NonDet
detConj SemiDet _ = SemiDet
detConj _ SemiDet = SemiDet
detConj Det Det   = Det

detDisj DetFail y = y
detDisj x DetFail = x
detDisj _ _       = NonDet

detDisjointDisj DetFail y       = y
detDisjointDisj x DetFail       = x
detDisjointDisj NonDet _        = NonDet
detDisjointDisj _ NonDet        = NonDet
detDisjointDisj Det _           = Det
detDisjointDisj _ Det           = Det
detDisjointDisj SemiDet SemiDet = SemiDet

semify Det = SemiDet
semify d = d

getDet :: DetState -> Lang -> DetType
getDet _ Empty = DetFail
getDet state (Call _ _ n _ _) = Det `fromMaybe` Map.lookup n state
getDet _ (Return _) = Det
getDet state (Sum xs) = foldl (if disjointCheck xs then detDisjointDisj else detDisj) DetFail (map (getDet state) xs)
getDet state (Match _ (_, x)) = semify (getDet state x)
getDet state (Bind xs) = foldl detConj Det (map (\(_, l) -> getDet state l) xs)
getDet _ (Guard _ _) = SemiDet
getDet _ (Gen _) = NonDet

validDet NonDet = False
validDet _ = True

putDet :: DetState -> Def -> DetState
putDet state d = Map.insert (name d) (getDet state (body d)) state

updateState :: DetState -> [Def] -> DetState
updateState state p = foldl putDet state p

fixState :: DetState -> [Def] -> DetState
fixState state p = let state' = updateState state p in if state == state' then state else fixState state' p

detCheck :: [Def] -> DetState
detCheck = fixState (Map.empty)

disjointCheck' :: Var -> Set.Set Name -> Lang -> Maybe (Set.Set Name)
disjointCheck' x con (Guard x' (Con y _))   | x == x' = if Set.member y con then Nothing else Just $ Set.insert y con
disjointCheck' x con (Match x' (Con y _,_)) | x == x' = if Set.member y con then Nothing else Just $ Set.insert y con
disjointCheck' x con (Bind ((_, p):_)) = disjointCheck' x con p
disjointCheck' _ _ _ = Nothing

findDisjVar :: Lang -> Maybe Var
findDisjVar (Guard x _) = return x
findDisjVar (Match x _) = return x
findDisjVar (Bind ((_,p):_)) = findDisjVar p
findDisjVar _ = Nothing

disjointCheck :: [Lang] -> Bool
disjointCheck [] = True
disjointCheck xs@(x:_) = isJust $ do
    v <- findDisjVar x
    foldM (disjointCheck' v) Set.empty xs


class DetQuotable a q | a -> q where
  toDetQuote :: DetType -> a -> Either Error q

instance DetQuotable Def TH.Dec where
  toDetQuote det d = do
    ds <- go d
    return $ TH.FunD (TH.mkName (name d)) [ds]
    where
      go :: Def -> Either Error TH.Clause
      go (Def _ args gens body _) = do
        args <- mapM pvar args
        gens <- mapM pgen gens
        b <- toDetQuote det body
        return $ TH.Clause (args ++ gens) (TH.NormalB b) []

detc det | validDet det = TH.AppE (TH.ConE $ TH.mkName "Det")
         | otherwise = id

instance DetQuotable Lang TH.Exp where
  toDetQuote det (Return exprs) = do
    fn <- qname "return"
    let dn = (TH.ConE $ TH.mkName "Det")
    exprs <- mapM toQuote exprs
    let e = case exprs of
          [e] -> e
          es -> TH.TupE (Just <$> es)
    return $ (if validDet det then dn else fn) $: [e]
  toDetQuote det (Sum exprs) = do
    fn <- qname "msum"
    exprs <- mapM (toDetQuote det) exprs
    return $ fn $: [TH.ListE exprs]
  toDetQuote det (Bind [([], ret)]) = do
    r <- toDetQuote det ret
    return $ r
  toDetQuote det (Bind stmts) = do
    let mainBody = init stmts
    let ([], tailRet) = last stmts
    xs <- mapM go mainBody
    xr <- toDetQuote det tailRet
    return $ TH.DoE Nothing (xs ++ [TH.NoBindS xr])
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
  toDetQuote _ l = toQuote l

newtype DetProgram = DetProgram Program

instance Quotable DetProgram ProgramDec where
  toQuote (DetProgram (Program types defs goal)) = do
    let det = detCheck defs
    t <- toQuote types
    d <- mapM (\df -> toDetQuote (fromMaybe NonDet $ Map.lookup (name df) det) df) defs
    b <- traverse toQuote goal
    return $ ProgramDec (t:d) b

embedProgDet :: String -> Program -> Either Error [TH.Dec]
embedProgDet n p@(Program _ _ call) = do
  (ProgramDec decs body) <- toQuote (DetProgram p)
  call <- callToDec n call body
  return $ decs ++ call