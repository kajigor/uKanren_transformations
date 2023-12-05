{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module FunConversion.Trans where

import qualified Data.Set as Set
import qualified FunConversion.Syntax as F
import qualified Mode.NormSyntax as M
import qualified Mode.Term as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Syntax as S
import Data.Foldable (fold)
import Data.List (nub, sort)
import Def
import           Mode.Inst
import           Program
import qualified Mode.Toplevel as M
import qualified Mode.Analysis as M
import Data.Bifunctor (second)
import Data.Maybe (maybeToList, fromMaybe)
import qualified FunConversion.DetMode as D
import Debug.Trace

pattern In :: Mode
pattern In = Mode Ground (Just Ground)

pattern Out :: Mode
pattern Out = Mode Free (Just Ground)

pattern InV :: a -> M.Var (a, Mode)
pattern InV v = M.Var (v, In)

pattern OutV :: a -> M.Var (a, Mode)
pattern OutV v = M.Var (v, Out)

mcons :: Maybe a -> [a] -> [a]
mcons m l = maybeToList m ++ l

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

getDefGens n [] = error "Def not found"
getDefGens n (d : defs)
  | n == F.name d = F.generators d
  | otherwise = getDefGens n defs


transProg :: String -> [S.S] -> Program S.G S.X -> Either M.ModeAnalysisError F.Program
transProg n inns p = do
  p' <- M.topLevelWithDefaultCall p n inns
  let (defs, body) = trans p'
  let types = collectConsPrg p'
  return $ F.Program types defs (Just body)

topLevel :: [S.S] -> Program S.G S.X -> Either M.ModeAnalysisError F.Program
topLevel inns p = do
  p' <- M.topLevel p inns
  let (defs, body) = trans p'
  let types = collectConsPrg p'
  return $ F.Program types defs (Just body)


transMultiMode :: [Def S.G S.X] -> [(String, [S.S])] -> Either M.ModeAnalysisError F.Program
transMultiMode defs modes = do
  p' <- M.topLevelManyModes defs modes
  let dets = traceShowId $ D.checkDefs $ D.detcheck' p'
  let defs' = transDefs dets p'
  let types = F.TypeData $ Set.toList $ collecCons p'
  return $ F.Program types defs' Nothing


transSingleMode :: [Def S.G S.X] -> (String, [S.S]) -> Either M.ModeAnalysisError F.Program
transSingleMode defs (name, ground) = do
  p' <- M.topLevelWithDefaultCall (Program defs undefined) name ground
  let (defs, body) = trans p'
  let types = collectConsPrg p'
  return $ F.Program types defs (Just body)

delay = F.Delayed -- we can delay everything which is stream

trans :: Program M.Goal (S.S, Mode) -> ([F.Def], F.Lang)
trans p@(Program defs (M.Disj ((M.Conj ((M.Call n args) NE.:| [])) NE.:| []))) =
  let
    defs' = transDefs dets defs
    rel = nameFromArgs n args
  in (defs', F.Call delay conversion rel (callVars args) (getDefGens rel defs'))
  where
    dets = traceShowId $ D.checkDefs $ D.detcheck' defs
    topLevelDet = D.isDet (D.identify' n (D.devar <$> args)) dets
    conversion = if topLevelDet then F.FromMaybe else F.NoConversion
trans _ = error "Expected single call"

transDefs :: D.DetMap -> [Def M.Goal (S.S, Mode)] -> [F.Def]
transDefs dets defs = fixGens $ map (\d -> transDef dets (outIdPair $ M.Var <$> getArgs d) d) defs

fixGens :: [F.Def] -> [F.Def]
fixGens defs = let defs' = collectGens defs in if defs == defs' then defs' else fixGens defs'

collectGens :: [F.Def] -> [F.Def]
collectGens defs = let s1 = map ownGens defs in map (updateCalls s1) s1

ownGens :: F.Def -> F.Def
ownGens d = d { F.generators = sort $ ownGens' (F.body d) }
  where
    ownGens' :: F.Lang -> [F.Generator]
    ownGens' (F.Gen g) = [g]
    ownGens' (F.Call _ _ _ _ gens) = gens
    ownGens' (F.Sum s) = nub $ s >>= ownGens'
    ownGens' (F.Bind b) = nub $ b >>= (\(_, l) -> ownGens' l)
    ownGens' (F.Match _ (_, l)) = nub $ ownGens' l
    ownGens' _ = []

updateCalls :: [F.Def] -> F.Def -> F.Def
updateCalls defs d = d { F.body = updateCalls' defs (F.body d) }
  where
    updateCalls' :: [F.Def] -> F.Lang -> F.Lang
    updateCalls' defs (F.Call d c n args _) = F.Call d c n args (getDefGens n defs)
    updateCalls' defs (F.Sum s) = F.Sum (map (updateCalls' defs) s)
    updateCalls' defs (F.Bind b) = F.Bind (map (second (updateCalls' defs)) b)
    updateCalls' defs (F.Match x m) = F.Match x ((second (updateCalls' defs)) m)
    updateCalls' _ l = l

outId :: [M.Var (S.S, Mode)] -> [S.S]
outId [] = []
outId (OutV v : vs) = 0 : map (+1) (outId vs)
outId (InV v : vs) = map (+1) (outId vs)
outId _ = []

outIdPair :: [M.Var (S.S, Mode)] -> [(S.S, S.S)]
outIdPair [] = []
outIdPair (OutV v : vs) = (0, v) : map (\(i, x) -> (i+1, x)) (outIdPair vs)
outIdPair (InV v : vs) = map (\(i, x) -> (i+1, x)) (outIdPair vs)
outIdPair _ = []

-- TODO: Check for same constructor with different arity
collectConsPrg :: Program M.Goal (S.S, Mode) -> F.TypeData
collectConsPrg (Program defs body) = F.TypeData (Set.toList $ collectDisjCons body <> collecCons defs)

collecCons :: [Def M.Goal (S.S, Mode)] -> Set.Set (String, Int)
collecCons defs = mconcat $ map (\(Def _ _ body) -> collectDisjCons body) defs

collectDisjCons :: M.Disj (S.S, Mode) -> Set.Set (String, Int)
collectDisjCons (M.Disj (x NE.:| xs)) = mconcat $ map collectConjCons (x:xs)

collectConjCons :: M.Conj (S.S, Mode) -> Set.Set (String, Int)
collectConjCons (M.Conj (x NE.:| xs)) = mconcat $ map convertCons (x:xs)

convertCons :: M.Base (S.S, Mode) -> Set.Set (String, Int)
convertCons (M.Unif _ (M.FTCon n args)) = Set.singleton (n, length args)
convertCons _ = Set.empty

outVarsV :: M.Var (a, Mode) -> [a]
outVarsV (OutV v) = [v]
outVarsV _ = []

outVarsT :: M.FlatTerm (a, Mode) -> [a]
outVarsT (M.FTVar v) = outVarsV v
outVarsT (M.FTCon _ xs) = xs >>= outVarsV

outVarsG :: M.Base (a, Mode) -> [a]
outVarsG (M.Unif a b) = outVarsV a ++ outVarsT b
outVarsG (M.Call _ xs) = xs >>= outVarsV

transBind :: D.DetMap -> D.DefIdentifier -> M.Base (S.S, Mode) -> ([F.Var], F.Lang)
transBind dets rel g = (map makeName (outVarsG g), transBase dets rel g)

mapVars :: (a -> Maybe b) -> (a -> Maybe b) -> [M.Var (a, Mode)] -> [b]
mapVars _ _ [] = []
mapVars inV outV ((InV v):vs) = inV v `mcons` mapVars inV outV vs
mapVars inV outV ((OutV v):vs) = outV v `mcons` mapVars inV outV vs
mapVars _ _ _ = error "Invalid mode"

callVars :: [M.Var (S.S, Mode)] -> [F.Var]
callVars = mapVars (Just . makeName) (const Nothing)

genVars :: [M.Var (S.S, Mode)] -> [F.Generator]
genVars = mapVars (const Nothing) (Just . makeName)

returnVars :: [M.Var (S.S, Mode)] -> [F.Term]
returnVars = mapVars (const Nothing) (Just . F.Var . makeMatchName)

bindGuards :: [M.Var (S.S, Mode)] -> [F.Lang]
bindGuards = mapVars (\v' -> Just $ F.Guard (makeName v') (F.Var $ makeMatchName v')) (const Nothing)

isIn :: M.FlatTerm (S.S, Mode) -> Bool
isIn (M.FTVar (InV _)) = True
isIn (M.FTCon _ []) = True
isIn (M.FTCon s ((InV _):vs)) = isIn (M.FTCon s vs)
isIn _ = False


makeGuard :: S.S -> F.Term -> F.Lang
makeGuard a b = F.Guard (makeName a) b

makeGen :: D.DefIdentifier -> F.Var -> ([F.Var], F.Lang)
makeGen rel x = ([x], F.Gen ("gen_" ++ (idToName rel) ++ "_" ++ x))

modeToSymbol :: Mode -> String
modeToSymbol In = "I"
modeToSymbol Out = "O"

idToName :: D.DefIdentifier -> String
idToName (D.DId (name, modes)) = name ++ concat (modeToSymbol <$> modes)

transBase :: D.DetMap -> D.DefIdentifier -> M.Base (S.S, Mode) -> F.Lang
transBase _ _ (M.Call "fail" []) = F.Empty
transBase dets rel (M.Call n args) = F.Call delay conversion (nameFromArgs n args) (callVars args) [] -- Generators are filled in later, by fixGens
  where
    selfDet = D.isDet rel dets
    callDet = D.isDet (D.identify' n (D.devar <$> args)) dets
    delay | not callDet = F.Delayed 
          | otherwise = F.NotDelayed
    conversion | callDet && (not selfDet) = F.FromMaybe
               | otherwise = F.NoConversion
transBase _ _ (M.Unif (OutV v) t) | isIn t = F.Return [makeTerm t]
transBase _ _ (M.Unif (InV v) (M.FTVar (OutV t))) = F.Return [makeVar v]
transBase _ _ (M.Unif (InV v) t) | isIn t = makeGuard v (makeTerm t)
transBase _ _ (M.Unif (InV v) t@(M.FTCon n xs)) = F.Match (makeName v) (F.Con n (map makeMatch xs), body)
  where
    body = F.Bind (guards ++ [([], F.Return (returnVars xs))])
    guards = map ([],) (bindGuards xs)
transBase _ rel (M.Unif (OutV v) (M.FTVar (OutV t))) = F.Bind [
      makeGen rel x
    , ([], F.Return [F.Var x, F.Var x])
    ]
    where
      x = makeName t
transBase _ rel (M.Unif (OutV v) t@(M.FTCon n xs)) = F.Bind
  (gen ++ [([makeName v], F.Return [makeTerm t]), ([], F.Return $ makeVar v : map F.Var genv)])
  where
      genv = genVars xs
      gen = map (makeGen rel) genv
transBase _ _ g = error $ "Unknown Base: " ++ show g

boundVars :: M.Conj (S.S, Mode) -> Set.Set S.S
boundVars (M.Conj xs) = fold (NE.map (Set.fromList . outVarsG) xs)

unboundVars :: [S.S] -> M.Conj (S.S, Mode) -> [S.S]
unboundVars outs r = let s = boundVars r in filter (`Set.notMember` s) outs

transConj :: D.DetMap -> D.DefIdentifier -> [S.S] -> M.Conj (S.S, Mode) -> F.Lang
transConj dets rel outs r@(M.Conj xs) = F.Bind $ body ++ gen ++ [([], F.Return (map makeVar outs))]
  where
    body = map (transBind dets rel) (NE.toList xs)
    gen = map (makeGen rel . makeName) (unboundVars outs r) -- Generators for out-vars that are not bound in any way

transDisj :: D.DetMap -> D.DefIdentifier -> [S.S] -> M.Disj (S.S, Mode) -> F.Lang
transDisj dets rel outs (M.Disj xs) = F.Sum $ map (transConj dets rel outs) (NE.toList xs)

transGoal :: D.DetMap -> D.DefIdentifier -> [S.S] -> M.Goal (S.S, Mode) -> F.Lang
transGoal = transDisj

transDef :: D.DetMap -> [(S.S, S.S)] -> Def M.Goal (S.S, Mode) -> F.Def
transDef dets outs (Def n args body) =
    let n' = makeDefName n (length args) (map fst outs) in
    let did = D.identify' n args in
    F.Def {
      F.name = n',
      F.args = callVars $ M.Var <$> args,
      F.generators = [], -- Generators are filled in later, by fixGens
      F.body = transGoal dets did (map snd outs) body,
      F.isSemidet = isSemidet did
    }
  where
    isSemidet did = fromMaybe False $ Map.lookup did dets

mapOuts :: (Int -> Maybe a) -> (Int -> Maybe a) -> Int -> [S.S] -> [a]
mapOuts i o n outs = mapOuts' i o n outs 0
  where
    mapOuts' :: (Int -> Maybe a) -> (Int -> Maybe a) -> Int -> [S.S] -> Int -> [a]
    mapOuts' _ _ 0 _ _ = []
    mapOuts' i o n (0:outs) k = o k `mcons` mapOuts' i o (pred n) (map pred outs) (k + 1)
    mapOuts' i o n outs k = i k `mcons` mapOuts' i o (pred n) (map pred outs) (k + 1)


ins :: Int -> [S.S] -> [S.S]
ins = mapOuts Just (const Nothing)

makeDefName :: String -> Int -> [S.S] -> String
makeDefName name n outs = name ++ mapOuts (const $ Just 'I') (const $ Just 'O') n outs

nameFromArgs :: String -> [M.Var (S.S, Mode)] -> String
nameFromArgs name args = makeDefName name (length args) (outId args)