
module FunConversion.Updated.Trans(translateDefs, translateProg, translateSingleMode, translateMultiMode) where
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import qualified Syntax as S
import qualified Mode.NormSyntax as M
import qualified Mode.Term as M
import           Mode.Inst
import qualified Def as Def
import Def (getArgs, getBody)

import qualified FunConversion.DetMode as D
import FunConversion.DetMode (DefIdentifier, identifyDef')

import qualified FunConversion.Updated.Syntax as F
import FunConversion.Updated.Utils
import Program

import qualified Mode.Toplevel as M

type Def = Def.Def M.Goal MV

data TranslationError
    = HeterogenousUnification MTerm deriving (Show, Eq)

type Error = Either String

ensureHomogenity :: [Def] -> Error ()
ensureHomogenity = mapM_ (M.walkDisjM_ goBase . getBody)
    where
        goBase (M.Unif _ y) = checkTerm y
        goBase _ = return ()

        checkTerm (M.FTVar _) = return ()
        checkTerm (M.FTCon _ []) = return ()
        checkTerm t@(M.FTCon _ (x:xs)) = lguard (show $ HeterogenousUnification t) $ all ((mode x ==) . mode) xs


isOutTerm :: MTerm -> Bool
isOutTerm (M.FTVar (OutV _)) = True
isOutTerm (M.FTCon _ (OutV _:_)) = True
isOutTerm _ = False

isInTerm :: MTerm -> Bool
isInTerm (M.FTVar (InV _)) = True
isInTerm (M.FTCon _ []) = True
isInTerm (M.FTCon _ (InV _:_)) = True
isInTerm _ = False

type GenMap = Map.Map DefIdentifier [F.Generator]

unboundGens :: DefIdentifier -> [MV] -> M.Conj MV -> Set.Set F.Generator
unboundGens def args c = Set.mapMonotonic (\v -> (def, F.NormalVar v)) $ unboundVars args c

unboundVars :: [MV] -> M.Conj MV -> Set.Set S.S
unboundVars args c = (filterVars $ Set.fromList args) `Set.difference` boundVars
    where
        boundVars = Set.unions $ M.walkConj (filterVars . go) c
            where
                go (M.Call _ _ args) = Set.fromList $ M.getVar <$> args
                go (M.Unif (M.Var v) t) = Set.insert v $ M.varsFromTerm t

        filterVars = Set.mapMonotonic (\(v,_) -> v) . Set.filter (\(_,m) -> m == Out)

computeGens :: [Def] -> GenMap
computeGens defs = Set.toAscList <$> simpleFixpoint updateDefs Map.empty
    where
        updateDefs gens = foldr updateDef gens defs
        updateDef d gens = Map.insert did findGens gens
            where
                did = identifyDef' d
                findGens = Set.union properGens improperGens
                
                properGens = Set.unions $ M.walkDisj baseGen $ getBody d
                improperGens = let (M.Disj cs) = getBody d in Set.unions $ unboundGens did (getArgs d) <$> (NE.toList cs)

                baseGen (M.Unif (OutV v) t) | isOutTerm t = Set.singleton (did, makeVar v)
                baseGen (M.Call _ name args) = fromMaybe Set.empty $ Map.lookup (identifyCall name args) gens
                baseGen _ = Set.empty 



data TranslateInfo = TI { detInfo :: D.DetMap, genInfo :: GenMap }

isDet :: DefIdentifier -> TranslateInfo -> Bool
isDet d i = D.isDet d (detInfo i)

getGens :: DefIdentifier -> TranslateInfo -> [F.Generator]
getGens d i = fromMaybe [] $ Map.lookup d (genInfo i)

delay :: M.Delayed -> F.Delayed
delay M.NotDelayed = F.NotDelayed
delay M.Delayed = F.Delayed

mcons :: Maybe a -> [a] -> [a]
mcons (Just x) xs = x:xs
mcons Nothing xs = xs

mapVars :: (a -> Maybe b) -> (a -> Maybe b) -> [M.Var (a, Mode)] -> [b]
mapVars _ _ [] = []
mapVars inV outV ((InV v):vs) = inV v `mcons` mapVars inV outV vs
mapVars inV outV ((OutV v):vs) = outV v `mcons` mapVars inV outV vs
mapVars _ _ _ = error "Invalid mode"

makeVar :: S.S -> F.Var
makeVar = F.NormalVar

makeVar' :: M.Var (S.S, a) -> F.Var
makeVar' (M.Var (v, _)) = makeVar v

makeTerm :: M.FlatTerm (S.S, a) -> F.Term
makeTerm (M.FTVar v) = F.Var $ makeVar' v
makeTerm (M.FTCon name xs) = F.Con name (map makeVar' xs)
        

inVars :: [M.Var MV] -> [F.Var]
inVars = mapVars (Just . makeVar) (const Nothing)

outVars :: [M.Var MV] -> [F.Var]
outVars = mapVars (const Nothing) (Just . makeVar)

collectCons :: [Def] -> F.TypeData
collectCons defs = F.TypeData (Set.toList $ collecConsDef defs)
    where
        collecConsDef = Set.unions . ((collectConsGoal . getBody) <$>)

        collectConsGoal = Set.unions . M.walkDisj collectConsBase

        collectConsBase (M.Unif _ (M.FTCon n args)) = Set.singleton (n, length args)
        collectConsBase _ = Set.empty

translate :: TranslateInfo -> Def -> F.Def
translate info def = F.Def 
    { F.name = caller
    , F.args = inVars $ M.Var <$> args
    , F.generators = getGens caller info
    , F.body = translateDisj (getBody def)
    }
    where
        caller = identifyDef' def
        args = getArgs def

        translateDisj (M.Disj xs) = F.Sum $ translateConj <$> xs
        translateConj c@(M.Conj xs) = F.Bind (body `NE.appendList` gens) ret
            where
                body = translateBase <$> xs
                gens = (\g@(_,v) -> F.Gen g (F.Var v)) <$> (Set.toList $ unboundGens caller args c)
                ret = outVars $ M.Var <$> args
        
        translateBase (M.Unif (InV  v) t) | isInTerm t = F.Guard (makeVar v) (makeTerm t)
        translateBase (M.Unif (OutV v) t) | isInTerm t = F.Assn (makeTerm t) (F.Var $ makeVar v)
        translateBase (M.Unif (InV  v) t)              = F.Assn (F.Var $ makeVar v) (makeTerm t)
        translateBase (M.Unif (OutV v) t)              = F.Gen (caller, makeVar v) (makeTerm t)
        translateBase (M.Call _ "fail" [])             = F.Empty
        translateBase (M.Call d n params)              = F.Call (delay d) conversion callee params' gens rets
            where
                callee = identifyCall n params
                conversion | (isDet callee info) && (not $ isDet caller info) = F.FromMaybe
                        | otherwise = F.NoConversion
                params' = inVars params
                gens = getGens callee info
                rets = outVars params

translateDefs :: [Def] -> Either String (F.TypeData, [F.Def])
translateDefs defs = do
    ensureHomogenity defs
    let info = TI { detInfo = D.checkDefs $ D.detcheck' defs, genInfo = computeGens defs }
    let types = collectCons defs
    let defs' = translate info <$> defs
    return (types, defs')

translateProg :: Program M.Goal MV -> Either String F.Program
translateProg (Program defs (M.Disj ((M.Conj ((M.Call _ n args) NE.:| [])) NE.:| []))) = do
    (types, defs') <- translateDefs defs
    return $ F.Program types defs' (Just $ identifyCall n args)
translateProg (Program defs _) = do
    (types, defs') <- translateDefs defs
    return $ F.Program types defs' Nothing


translateSingleMode :: String -> [S.S] -> Program S.G S.X -> Either String F.Program
translateSingleMode rel ground p = do
    p' <- M.topLevelWithDefaultCall p rel ground
    translateProg p'

translateMultiMode :: [Def.Def S.G S.X] -> [(String, [S.S])] -> Either String F.Program
translateMultiMode defs modes = do
    defs' <- M.topLevelManyModes defs modes
    (types, defs'') <- translateDefs defs'
    return $ F.Program types defs'' Nothing