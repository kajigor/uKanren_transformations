{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
module FunConversion.DetMode where
import qualified Data.Set as Set
import qualified Mode.NormSyntax as M
import qualified Mode.Term as M
import qualified Mode.Toplevel as M
import qualified Mode.Analysis as M
import qualified Data.List.NonEmpty as NE
import qualified Mode.Inst as M
import qualified Mode.Pretty as P
import qualified Subst as Subst
import qualified Syntax as S
import qualified Data.Map as Map
import Eval (unifyAll)

import Data.Maybe (isNothing, fromJust)
import Def
import Program
import Text.Printf
import Prettyprinter (viaShow)
import Debug.Trace

import Control.Monad.State

pattern DV' :: a -> DetMode -> M.Mode -> (a, MixedMode)
pattern DV' v d g = (v, MMode d g)

pattern DV :: a -> DetMode -> M.Mode -> M.Var (a, MixedMode)
pattern DV v d g = M.Var (v, MMode d g)

data DetInst = Det | NonDet deriving (Eq, Ord)

instance Show DetInst where

    show Det = "d"
    show NonDet = "-"


data DetMode = DMode { before :: DetInst, after :: DetInst } deriving (Eq, Ord)

pattern WasDet = DMode Det Det
pattern BecameDet = DMode NonDet Det
pattern NotDet = DMode NonDet NonDet

instance Show DetMode where
  show mode =
      printf "%s -> %s" (show $ before mode) (show $ after mode)

data MixedMode = MMode { det :: DetMode, grd :: M.Mode } deriving (Eq, Ord)

instance Show MixedMode where

    show (MMode d g) = 
        printf "(%s, %s) -> (%s, %s)"  (show $ before d) (show $ M.before g) (show $ after d) (showAfter $ M.after g)
        where
            showAfter Nothing = "_"
            showAfter (Just inst) = show inst

instance P.ShowPretty MixedMode where
    showPretty = return . viaShow

devar :: M.Var a -> a
devar (M.Var v) = v

makeVarDet :: (Ord a) => Set.Set a -> DetMode -> (a, MixedMode) -> (a, MixedMode)
makeVarDet v m x@(DV' u (DMode _ NonDet) g) | u `elem` v = DV' u m g
makeVarDet v m x = x

makeDet :: (Ord a) => Set.Set a -> DetMode -> [M.Base (a, MixedMode)] -> [M.Base (a, MixedMode)]
makeDet v m = map (\x -> makeVarDet v m <$> x)

isBeforeDet :: (a, MixedMode) -> Bool
isBeforeDet (DV' _ d _) = before d == Det

type MDef a = Def M.Goal (a, MixedMode)

updateDetMode :: (Ord a) => [MDef a] -> [MDef a]
updateDetMode = (updateDef <$>)
    where
        updateDef (Def name args body) = Def name args $ updateGoal body

        updateGoal (M.Disj disj) = M.Disj $ updateConj <$> disj

        updateConj (M.Conj conj) = M.Conj $ NE.fromList $ updateList $ NE.toList conj

        updateList xs = 
            let
                (todet, xs') = forwardPass xs
            in makeDet todet WasDet xs'

        forwardPass [] = (Set.empty, [])
        forwardPass (o@(M.Unif (M.Var x@(DV' v _ _)) t):xs) = 
            let 
                (after, rest) = forwardPass xs

                rhsVars = M.varsFromTerm t
                rhs = if (isBeforeDet x) then Set.mapMonotonic fst rhsVars else Set.empty
                lhs = if (not (Set.null rhsVars) && all isBeforeDet rhsVars) then Set.insert v else id
                todet = lhs rhs

            in (Set.union todet after, (makeVarDet todet BecameDet <$> o) : rest)
        forwardPass (x:xs) = (x:) <$> forwardPass xs

introduceDetMode :: (Ord a) => [Def M.Goal (a, M.Mode)] -> [MDef a]
introduceDetMode = (updateDef <$>)
    where
        updateDef (Def name args body) = 
            let
                inargs = Set.fromList $ fst <$> filter M.isBeforeGround args
                addMode = \(v, g) -> DV' v (if v `elem` inargs then WasDet else NotDet) g
            in Def name (addMode <$> args) (addMode <$> body)

simpleFixpoint :: (Eq x) => (x -> x) -> x -> x
simpleFixpoint f x = let x' = f x in if x' == x then x' else simpleFixpoint f x'

runDetCheck :: (Ord a) => [MDef a] -> [MDef a]
runDetCheck = simpleFixpoint updateDetMode

detcheck' :: (Ord a) => [Def M.Goal (a, M.Mode)] -> [MDef a]
detcheck' = runDetCheck . introduceDetMode

detcheck :: (Ord a) => Program M.Goal (a, M.Mode) -> Program M.Goal (a, MixedMode)
detcheck (Program defs goal) = Program (detcheck' defs) ((\(v, g) -> DV' v NotDet g) <$> goal)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = ((x,) <$> xs) ++ pairs xs


isExclusive :: (Show a, Ord a) => MDef a -> Bool
isExclusive (Def name args (M.Disj disj)) = all (\(f, g) -> isExclusiveConj f g) (pairs $ NE.toList disj)
    where
        isExclusiveConj f g = 
            let
                uf = collectDetUnifs f
                ug = collectDetUnifs g
                total = uf ++ ug
            in isNothing $ unifyAll (Just Subst.empty) ((\(M.Unif x t) -> (M.varToPlainTerm x, M.toPlainTerm t)) <$> total)

        collectDetUnifs (M.Conj body) = map (fst <$>) $ filter isDetUnif (NE.toList body)

        isDetUnif (M.Unif (DV _ (DMode { after = Det }) _) _) = True
        isDetUnif _ = False


containsVarGenerators :: MDef a -> Bool
containsVarGenerators (Def _ _ (M.Disj disj)) = any check disj
    where
        check (M.Conj body) = any isGen body

        isGen (M.Unif (DV _ _ (M.Mode { M.before = M.Free })) (M.FTVar (DV _ _ (M.Mode { M.before = M.Free })))) = True
        isGen _ = False

boundVars :: (Ord a) => M.Goal a -> Set.Set a
boundVars (M.Disj xs) = foldr1 Set.intersection (go <$> xs)
    where

        go (M.Conj body) = foldr1 Set.union (getVars <$> body)

        getVars (M.Unif v t) = Set.insert (devar v) (M.varsFromTerm t)
        getVars (M.Call _ _ args) = Set.fromList (devar <$> args)

boundVars' :: (Ord a) => M.Goal (a, MixedMode) -> Set.Set a
boundVars' = boundVars . (fst <$>)

hasUnboundArgs :: (Ord a) => MDef a -> Bool
hasUnboundArgs (Def _ args body) = let vs = boundVars' body in any (\(DV' v _ g) -> M.before g == M.Free && v `Set.notMember` vs) (args)

newtype DefIdentifier = DId (String, [M.Mode]) deriving (Show, Eq, Ord)
type DetCheckState = Map.Map DefIdentifier (Maybe Bool) -- Nothing - internal only, for tracking recursion

identify :: String -> [(a, MixedMode)] -> DefIdentifier
identify name args = DId (name, (grd . snd) <$> args)

identifyDef:: MDef a -> DefIdentifier
identifyDef (Def name args _) = identify name args

identify' :: String -> [(a, M.Mode)] -> DefIdentifier
identify' name args = DId (name, snd <$> args)

identifyDef' :: Def M.Goal (a, M.Mode) -> DefIdentifier
identifyDef' (Def name args _) = identify' name args


findDef :: [MDef a] -> DefIdentifier -> MDef a
findDef [] did = error $ "Def matching " ++ show did ++ " not found"
findDef (d:ds) did | identifyDef d == did = d
                   | otherwise = findDef ds did

collectCalls :: MDef a -> [DefIdentifier]
collectCalls (Def _ _ (M.Disj disj)) = (NE.toList disj) >>= collect
    where
        collect (M.Conj body) = getCalls (NE.toList body)

        getCalls [] = []
        getCalls ((M.Call _ name args):xs) = identify name (devar <$> args) : getCalls xs
        getCalls (_:xs) = getCalls xs 

checkDet :: (Show a, Ord a) => [MDef a] -> MDef a -> State DetCheckState Bool
checkDet defs d = do
    let did = identifyDef d
    det <- gets (Map.lookup did)
    case det of 
        Nothing -> do
            modify $ Map.insert did Nothing -- Mark for detecting recursion
            det' <- runCheck
            modify $ Map.insert did (Just $ det')
            return det'
        Just Nothing -> return True -- Recursion detected, recursion on a determenistic relation is deterministic, and non-deterministic relation will be detected by other criteria, so True here is correct
        Just (Just v) -> return v
    where
        runCheck = do
            let gens = not $ containsVarGenerators d
            let args = not $ hasUnboundArgs d
            let exclusivity = isExclusive d

            subcalls <- mapM (\did -> checkDet defs (findDef defs did)) (collectCalls d)
            let subcallsDet = all id subcalls
            
            return $ gens && args && exclusivity && subcallsDet

type DetMap = Map.Map DefIdentifier Bool

checkDefs :: (Show a, Ord a) => [MDef a] -> DetMap
checkDefs defs = fromJust <$> go
    where
        go = execState (mapM_ (checkDet defs) defs) Map.empty

isDet :: DefIdentifier -> DetMap -> Bool
isDet d dets = case Map.lookup d dets of
    Just b -> b
    Nothing -> False