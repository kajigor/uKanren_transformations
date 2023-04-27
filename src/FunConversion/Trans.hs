{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module FunConversion.Trans where

import qualified FunConversion.Syntax as F
import qualified Mode.NormSyntax as M
import qualified Mode.Term as M
import qualified Data.List.NonEmpty as NE
import qualified Syntax as S
import Data.List (nub, sort)
import Def
import           Mode.Inst
import           Program
import Mode.Toplevel (topLevelWithDefaultCall, topLevelManyModes)
import qualified Mode.Analysis as M
import Debug.Trace
import qualified Language.Haskell.TH as TH
import Data.Maybe (maybeToList)
pattern In :: Mode
pattern In = Mode Ground (Just Ground)

pattern Out :: Mode
pattern Out = Mode Free (Just Ground)

pattern Out' :: Mode
pattern Out' <- Mode Free _

pattern In' :: Mode
pattern In' <- Mode Ground _

pattern InV :: a -> M.Var (a, Mode)
pattern InV v = M.Var (v, In)

pattern OutV :: a -> M.Var (a, Mode)
pattern OutV v = M.Var (v, Out)

pattern InV' :: a -> M.Var (a, Mode)
pattern InV' v <- M.Var (v, In')

pattern OutV' :: a -> M.Var (a, Mode)
pattern OutV' v <- M.Var (v, Out')

makeName :: S.S -> F.Var
makeName n = "x" ++ show n

makeVar :: S.S -> F.Term
makeVar v = F.Var (makeName v)

makeMatchName :: S.S -> F.Var
makeMatchName n = "y" ++ show n

makeMatch :: M.Var (S.S, a) -> F.Var
makeMatch (M.Var (v, _)) = makeMatchName v

makeVar' :: M.Var (S.S, a) -> F.Var
makeVar' (M.Var (v, _)) = makeName v

makeTerm :: M.FlatTerm (S.S, a) -> F.Term
makeTerm (M.FTVar v) = F.Var $ makeVar' v
makeTerm (M.FTCon name xs) = F.Con name (map makeVar' xs)

findDef :: String -> [S.S] -> [Def M.Goal (S.S, Mode)] -> Def M.Goal (S.S, Mode)
findDef n outs [] = error $ "Def " ++ n ++ " mode " ++ show outs ++ " not found"
findDef n outs (d@(Def n' args _):ds) | n == n' && outId (map M.Var args) == outs = d
                              | otherwise = findDef n outs ds


transProg :: String -> [S.S] -> Program S.G S.X -> Either M.ModeAnalysisError F.Program
transProg n inns p = do
  p' <- topLevelWithDefaultCall p n inns
  let (defs, body) = trans p'
  let types = collectConsPrg p'
  return $ F.Program types defs (Just body)

transMultiMode :: [Def S.G S.X] -> [(String, [S.S])] -> Either M.ModeAnalysisError F.Program
transMultiMode defs modes = do
  p' <- topLevelManyModes defs modes
  let defs' = transDefs' p'
  let types = F.TypeData $ nub $ p' >>= collecCons
  return $ F.Program types defs' Nothing

transSingleMode :: [Def S.G S.X] -> (String, [S.S]) -> Either M.ModeAnalysisError F.Program
transSingleMode defs (name, ground) = do
  p' <- topLevelWithDefaultCall (Program defs undefined) name ground
  let (defs, body) = trans p'
  let types = collectConsPrg p'
  return $ F.Program types defs (Just body)

delay :: M.Delayed -> F.Delayed
delay M.NotDelayed = F.NotDelayed
delay M.Delayed = F.Delayed

trans :: Program M.Goal (S.S, Mode) -> ([F.Def], F.Lang)
trans p@(Program defs (M.Disj ((M.Conj ((M.Call d n args) NE.:| [])) NE.:| []))) = (
  fixGens $ transDefs defs (collectCallsPrg p),
  F.Call (delay d) (makeDefName (length args) (outId args) n) (callVars args) []
  )
trans _ = error "Expected single call"

trans' :: Program M.Goal (S.S, Mode) -> ([F.Def], F.Lang)
trans' p@(Program defs (M.Disj ((M.Conj ((M.Call d n args) NE.:| [])) NE.:| []))) = (
  fixGens $ transDefs' defs,
  F.Call (delay d) (makeDefName (length args) (outId args) n) (callVars args) []
  )
trans' _ = error "Expected single call"

transDefs' :: [Def M.Goal (S.S, Mode)] -> [F.Def]
transDefs' = map (\d -> transDef (outId $ M.Var <$> getArgs d) d)

fixGens :: [F.Def] -> [F.Def]
fixGens defs = let defs' = collectGens defs in if defs == defs' then defs' else fixGens defs'

collectGens :: [F.Def] -> [F.Def]
collectGens defs = let s1 = map ownGens defs in map (updateCalls s1) s1

ownGens :: F.Def -> F.Def
ownGens (F.Def n (args, _, l)) = F.Def n (args, sort (nub (ownGens' l)), l)
  where
    ownGens' :: F.Lang -> [F.Generator]
    ownGens' (F.Gen g) = [g]
    ownGens' (F.Call _ _ _ gens) = gens
    ownGens' (F.Sum s) = nub $ s >>= ownGens'
    ownGens' (F.Bind b) = nub $ b >>= (\(_, l) -> ownGens' l)
    ownGens' (F.Match _ m) = nub $ m >>= (\(_, l) -> ownGens' l)
    ownGens' _ = []

updateCalls :: [F.Def] -> F.Def -> F.Def
updateCalls defs (F.Def n (args, gens, l)) = F.Def n (args, gens, updateCalls' defs l)

updateCalls' :: [F.Def] -> F.Lang -> F.Lang
updateCalls' defs (F.Call d n args _) = F.Call d n args (getDefGens n defs)
  where
      getDefGens n [] = error "Def not found"
      getDefGens n (d@(F.Def n' (_, gens, _)) : defs) | n == n' = gens
                                                    | otherwise = getDefGens n defs
updateCalls' defs (F.Sum s) = F.Sum (map (updateCalls' defs) s)
updateCalls' defs (F.Bind b) = F.Bind (map (\(v, l) -> (v, updateCalls' defs l)) b)
updateCalls' defs (F.Match x m) = F.Match x (map (\(v, l) -> (v, updateCalls' defs l)) m)
updateCalls' _ l = l

-- TODO: Modcheck already gives directions
-- TODO: Костыль на объеденение модчеков
transDefs :: [Def M.Goal (S.S, Mode)] -> [(String, [S.S])] -> [F.Def]
transDefs defs = map (\(n, outs) -> transDef outs (findDef n outs defs))

collectDisjCalls :: M.Disj (S.S, Mode) -> [(String, [S.S])]
collectDisjCalls (M.Disj (x NE.:| xs)) = (x:xs) >>= collectConjCalls

collectConjCalls :: M.Conj (S.S, Mode) -> [(String, [S.S])]
collectConjCalls (M.Conj (x NE.:| xs)) = (x:xs) >>= convertCall

convertCall :: M.Base (S.S, Mode) -> [(String, [S.S])]
convertCall (M.Call _ n args) = [(n, outId args)]
convertCall _ = []

outId :: [M.Var (S.S, Mode)] -> [S.S]
outId [] = []
outId (OutV' v : vs) = 0 : map (+1) (outId vs)
outId (InV' v : vs) = map (+1) (outId vs)
outId _ = []

collectCalls :: Def M.Goal (S.S, Mode) -> [(String, [S.S])]
collectCalls (Def _ _ body) = collectDisjCalls body

collectCallsPrg :: Program M.Goal (S.S, Mode) -> [(String, [S.S])]
collectCallsPrg (Program defs body) = nub $ collectDisjCalls body ++ (defs >>= collectCalls)

-- TODO: Check for same constructor with different arity
collectConsPrg :: Program M.Goal (S.S, Mode) -> F.TypeData
collectConsPrg (Program defs body) = F.TypeData (nub $ collectDisjCons body ++ (defs >>= collecCons))

collecCons :: Def M.Goal (S.S, Mode) -> [(String, Int)]
collecCons (Def _ _ body) = collectDisjCons body

collectDisjCons :: M.Disj (S.S, Mode) -> [(String, Int)]
collectDisjCons (M.Disj (x NE.:| xs)) = (x:xs) >>= collectConjCons

collectConjCons :: M.Conj (S.S, Mode) -> [(String, Int)]
collectConjCons (M.Conj (x NE.:| xs)) = (x:xs) >>= convertCons

convertCons :: M.Base (S.S, Mode) -> [(String, Int)]
convertCons (M.Unif _ (M.FTCon n args)) = [(n, length args)]
convertCons _ = []

outVarsV :: M.Var (a, Mode) -> [a]
outVarsV (OutV' v) = [v]
outVarsV _ = []

outVarsT :: M.FlatTerm (a, Mode) -> [a]
outVarsT (M.FTVar v) = outVarsV v
outVarsT (M.FTCon _ xs) = xs >>= outVarsV

outVarsG :: (Eq a) => M.Base (a, Mode) -> [a]
outVarsG (M.Unif a b) = nub $ outVarsV a ++ outVarsT b
outVarsG (M.Call _ _ xs) = xs >>= outVarsV

transBind :: String -> M.Base (S.S, Mode) -> ([F.Var], F.Lang)
transBind rel g = (map makeName (outVarsG g), transBase rel g)

mapVars :: (M.Var (a, Mode) -> Maybe b) -> (M.Var (a, Mode) -> Maybe b) -> [M.Var (a, Mode)] -> [b]
mapVars _ _ [] = []
mapVars inV outV (v@(InV' _):vs) = maybeToList (inV v) ++ mapVars inV outV vs
mapVars inV outV (v@(OutV' _):vs) = maybeToList (outV v) ++ mapVars inV outV vs
mapVars _ _ _ = error "Invalid mode"

callVars :: [M.Var (S.S, Mode)] -> [F.Var]
callVars = mapVars (Just . makeVar') (const Nothing)

returnVars :: [M.Var (S.S, Mode)] -> [F.Term]
returnVars = mapVars (const Nothing) (Just . F.Var . makeMatch)

bindGuards :: [M.Var (S.S, Mode)] -> [F.Lang]
bindGuards = mapVars (\(InV v') -> Just $ F.Guard (makeName v') (makeMatchName v')) (const Nothing)

isIn :: M.FlatTerm (S.S, Mode) -> Bool
isIn (M.FTVar (InV _)) = True
isIn (M.FTCon _ []) = True
isIn (M.FTCon s ((InV _):vs)) = isIn (M.FTCon s vs)
isIn _ = False


makeGuard :: S.S -> S.S -> F.Lang
makeGuard a b = F.Guard (makeName a) (makeName b)

makeGen :: String -> F.Var -> ([F.Var], F.Lang)
makeGen rel x = ([x], F.Gen ("gen_" ++ rel ++ "_" ++ x))

-- makePattern :: Int -> [a] -> [F.Var]
-- makePattern _ [] = []
-- makePattern n (_:xs) = makeMatchName n : makePattern (n + 1) xs

-- TODO: Totally free out-variable
transBase :: String -> M.Base (S.S, Mode) -> F.Lang
transBase _ (M.Call d n args) = F.Call (delay d) (makeDefName (length args) (outId args) n) (callVars args) [] -- TODO: Immature
transBase _ (M.Unif (InV v) (M.FTVar (OutV' t))) = F.Return [makeVar v]
transBase _ (M.Unif (InV v) t'@(M.FTVar (InV t))) = makeGuard v t -- TODO: Replace with match?
transBase _ (M.Unif (InV v) t@(M.FTCon n xs)) = F.Match (makeName v) [
    (F.Con n (map makeMatch xs), F.Bind (map ([],) (bindGuards xs) ++ [([], F.Return (returnVars xs))]))
  ]
transBase _ (M.Unif (OutV' v) t) | isIn t = F.Return [makeTerm t]
transBase rel (M.Unif (OutV' v) (M.FTVar (OutV' t))) = let x = makeName t in F.Bind [makeGen rel x, ([], F.Return [F.Var x, F.Var x])] -- t <- genT; let v = t
transBase rel (M.Unif (OutV' v) t@(M.FTCon n xs)) = 
  let gens = mapVars (const Nothing) (\(OutV' v) -> Just $ makeName v) xs in F.Bind (
    map (makeGen rel) gens ++ [
      ([makeName v], F.Return [makeTerm t]),
      ([], F.Return $ makeVar v : map F.Var gens)
    ]
  )
transBase _ g = error $ "Unknown Base: " ++ show g

transConj :: String -> [S.S] -> M.Conj (S.S, Mode) -> F.Lang
transConj rel outs (M.Conj xs) = F.Bind (NE.toList (NE.map (transBind rel) xs) ++ [([], F.Return (map makeVar outs))])

transDisj :: String -> [S.S] -> M.Disj (S.S, Mode) -> F.Lang
transDisj rel outs (M.Disj xs) = F.Sum (NE.toList $ NE.map (transConj rel outs) xs)

transGoal :: String -> [S.S] -> M.Goal (S.S, Mode) -> F.Lang
transGoal = transDisj

transDef :: [S.S] -> Def M.Goal (S.S, Mode) -> F.Def
transDef outs (Def n args body) = let n' = makeDefName (length args) outs n in F.Def n' (map makeVar (ins (length args) outs), [], transGoal n' outs body)

ins :: Int -> [S.S] -> [S.S]
ins 0 _ = []
ins n [] = 0 : map (+1) (ins (pred n) [])
ins n (0:outs) = map (+1) (ins (pred n) (map pred outs))
ins n outs = 0 : map (+1) (ins (pred n) (map pred outs))

takeVars :: [S.S] -> [a] -> [a]
takeVars [] _ = []
takeVars _ [] = []
takeVars (0:outs) (x:xs) = x : takeVars (map pred outs) xs
takeVars outs (x:xs) = takeVars (map pred outs) xs

makeDefName :: Int -> [Int] -> String -> String
makeDefName 0 _ n = n
makeDefName k [] n = makeDefName (pred k) [] (n ++ "I")
makeDefName k (0:outs) n = makeDefName (pred k) (map pred outs) (n ++ "O")
makeDefName k outs n = makeDefName (pred k) (map pred outs) (n ++ "I")

disj :: [M.Conj a] -> M.Goal a
disj [] = error "Empty disj"
disj (x:xs) = M.Disj (x NE.:| xs)

conj :: [M.Base a] -> M.Conj a
conj [] = error "Empty disj"
conj (x:xs) = M.Conj (x NE.:| xs)

consExample :: M.Goal (S.S, Mode)
consExample = disj [ conj [ M.Unif (OutV 0) (M.FTCon "cons" [InV 1, OutV 2]) ] ] -- x2 <- genList; let x0 = x1 : x2 / x0 <- genList; case x0 (x3:x4) -> guard (x1 == x3); return x4


consExample' :: M.Goal (S.S, Mode)
consExample' = disj [ conj [ M.Unif (InV 0) (M.FTCon "cons" [InV 1, OutV 2]) ] ]-- case x0 /---/

zeroT :: S.Term a
zeroT = S.C "O" []

succT :: S.Term a -> S.Term a
succT x = S.C "S" [x]

addoDef :: Def S.G S.X
addoDef =
    Def "addo" ["x", "y", "z"]
        (
          x S.=== zeroT S.&&& z S.=== y S.|||
          S.fresh ["x'", "z'"]
            (z S.=== succT z' S.&&& S.call "addo" [x', y, z'] S.&&& x S.=== succT x')
        )
  where
    [x, y, z, x', z'] = map S.V ["x", "y", "z", "x'", "z'"]

muloDef :: Def S.G S.X
muloDef = Def "mulo" ["x", "y", "z"]
      (
        (x S.=== zeroT S.&&& z S.=== zeroT) S.|||
          S.fresh ["x'", "z'"]
            (x S.=== succT x' S.&&&
             S.call "mulo" [x', y, z'] S.&&&
             S.call "addo" [y, z', z])
      )
  where
    [x, y, z, x', z'] = map S.V ["x", "y", "z", "x'", "z'"]


testTrans :: IO ()
-- testTrans = print $ transProg "addo" [0, 1] (Program [addoDef, muloDef] (error "accesed original goal"))
testTrans = case F.toQuote <$> transProg "addo" [2] (Program [addoDef] (error "accesed original goal")) :: Either String (Either F.Error F.ProgramDec) of
  Left e -> print e
  Right (Left e) -> print e
  Right (Right (F.ProgramDec decs _)) -> do
    putStrLn (TH.pprint decs)
-- testTrans = putStrLn $ fromRight "" $ prettyString . M.back <$> topLevelWithDefaultCall (Program [addoDef] undefined) "addo" [0, 1]
-- testTrans = let (Right (Right x)) = F.toHaskell <$> transProg "mulo" [0, 2] (Program [addoDef, muloDef] (error "accesed original goal")) in print x
-- testTrans = let (Just p) = M.normalize <$> topLevelWithDefaultCall (Program [addoDef, muloDef] undefined) "addo" [2] in print (F.toHaskell (collectConsPrg p))
-- testTrans = let x = transDisj [2] consExample' in print (F.toHaskell x)

-- TODO: Merge matches
-- msum [ do (match (...), ...), do (match (...), ...) ] -> match (do ...) (do ...)

-- TODO: Benchmark
-- TODO: Tests - write hs file, run it/TemplateHaskell?
