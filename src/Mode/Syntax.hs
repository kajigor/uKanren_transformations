{-# LANGUAGE DeriveFunctor #-}
module Mode.Syntax where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Def
import           FreshNames
import           Mode.Term
import           Program
import qualified Syntax              as S
import           Text.Printf
import Debug.Trace

data Goal a = Call String [Var a]
            | Unif (Var a) (FlatTerm a)
            | Conj (Goal a) (Goal a) [Goal a]
            | Disj (Goal a) (Goal a) [Goal a]
            | EtaD (Goal a)
            deriving (Show, Eq, Ord, Functor)

allVars :: (Ord a, Eq a) => Goal a -> Set.Set a
allVars =
    go
  where
    go (Call _ args) = Set.fromList $ map getVar args
    go (Unif v t) = Set.insert (getVar v) $ varsFromTerm t
    go (Conj x y xs) = Set.unions $ map go (x:y:xs)
    go (Disj x y xs) = Set.unions $ map go (x:y:xs)
    go (EtaD g) = go g

freshVar :: (Ord a) => FreshName a => State (FlattenState a) a
freshVar = do
  state <- get
  let (v', rest) = getFreshName (FreshNames (getVarSource state))
  put $ state { getVarSource = unFreshNames rest, getFreeVars = Set.insert v' (getFreeVars state) }
  return v'

addTerm :: Ord a => a -> FlatTerm a -> State (FlattenState a) ()
addTerm key value = do
  bindVar key
  bindTerm value
  state <- get
  put $ state { getVarMap = Map.insert key value (getVarMap state)
              , getNewVarMap = Map.insert key value (getNewVarMap state)
              }

useVar :: Ord a => a -> State (UniqueVarFlattenState a) ()
useVar v = modify $ \s -> s { usedVars = Set.insert v (usedVars s) }

clearUsed :: State (UniqueVarFlattenState a) ()
clearUsed = modify $ \s -> s { usedVars = Set.empty }

bindVar :: Ord a => a -> State (FlattenState a) ()
bindVar x = modify $ \s -> s { getFreeVars = Set.delete x (getFreeVars s) }

bindTerm :: Ord a => FlatTerm a -> State (FlattenState a) ()
bindTerm (FTVar (Var v)) = bindVar v
bindTerm (FTCon _ args) = mapM_ (\(Var v) -> bindVar v) args

withUniqueVars :: Ord a => [a] -> State (UniqueVarFlattenState a) x -> State (FlattenState a) x
withUniqueVars vs act = do
  b <- get
  let (x, UniqueVar { baseState = b' }) = runState act (UniqueVar {usedVars = Set.fromList vs, baseState = b})
  put b'
  return x

withBaseState :: State (FlattenState a) x -> State (UniqueVarFlattenState a) x
withBaseState act = do
  b <- gets baseState
  let (x, b') = runState act b
  modify $ \s -> s { baseState = b' }
  return x

flattenTerm :: (Ord a, FreshName a) => S.Term a -> State (UniqueVarFlattenState a) a
flattenTerm (S.V v) = do
  used <- gets usedVars
  if v `elem` used then do
    newVar <- withBaseState freshVar
    withBaseState $ addTerm newVar (FTVar $ Var v)
    return newVar
  else do
    useVar v
    return v
flattenTerm (S.C name args) = do
  newVar <- withBaseState freshVar
  args' <- mapM flattenTerm args
  withBaseState $ addTerm newVar (FTCon name (map Var args'))
  return newVar

flattenInternalTerms :: (Ord a, FreshName a) => S.Term a -> State (UniqueVarFlattenState a) (FlatTerm a)
flattenInternalTerms (S.V v) = FTVar . Var <$> flattenTerm (S.V v)
flattenInternalTerms (S.C name args) = do
  args' <- mapM flattenTerm args
  return $ FTCon name $ map Var args'

conj :: Show a => Goal a -> Goal a
conj goal = trace (printf "\nin conj: %s" (show goal)) $
  safeListToConj $ floatConjs goal

disj :: Show a => Goal a -> Goal a
disj goal = trace (printf "\nin disj: %s" (show goal)) $
  safeListToDisj $ floatDisjs goal

success :: Goal a
success = Call "success" []

safeListToConj :: [Goal a] -> Goal a
safeListToConj (x:y:xs) = Conj x y xs
safeListToConj [x] = x
safeListToConj [] = success

safeListToDisj :: [Goal a] -> Goal a
safeListToDisj (x:y:xs) = Disj x y xs
safeListToDisj [x] = x
safeListToDisj [] = success

floatConjs :: Goal a -> [Goal a]
floatConjs (Conj x y xs) = concatMap floatConjs (x:y:xs)
floatConjs g = [g]

floatDisjs :: Goal a -> [Goal a]
floatDisjs (Disj x y xs) = concatMap floatDisjs (x:y:xs)
floatDisjs g = [g]

makeConjFromList :: (Ord a, FreshName a, Show a) => [Goal a] -> State (FlattenState a) (Goal a)
makeConjFromList goals = do
  state <- get
  put $ state { getNewVarMap = Map.empty }
  let newUnifs = map (\(x, t) -> Unif (Var x) t) $ Map.toList (getNewVarMap state)
  let allGoals = goals ++ newUnifs
  return $ conj $ safeListToConj allGoals

makeConj :: (Ord a, FreshName a, Show a) => Goal a -> State (FlattenState a) (Goal a)
makeConj goal =
  makeConjFromList [goal]

useFreeVar :: (Ord a, FreshName a) => (Var a) -> State (FlattenState a) (Var a)
useFreeVar (Var v) = do
  free <- gets getFreeVars
  if v `elem` free then do
    bindVar v
    return $ Var v
  else do
    newVar <- freshVar
    addTerm newVar (FTVar $ Var v)
    return $ Var newVar

homogenize :: (Ord a, FreshName a) => FlatTerm a -> State (FlattenState a) (FlatTerm a)
homogenize v@(FTVar _) = return v
homogenize (FTCon n vs) = FTCon n <$> mapM useFreeVar vs

transformUnification :: (Ord a, FreshName a, Show a) => S.Term a -> S.Term a -> State (FlattenState a) [Goal a]
transformUnification (S.C n1 args1) (S.C n2 args2)
  | n1 == n2 =
    let args1Length = length args1 in
    let args2Length = length args2 in
    if args1Length == args2Length
    then
      if args1Length == 0
      then
        return []
      else do
        transformed <- zipWithM transformUnification args1 args2
        return $ concat transformed
    else
      error $ printf "Cannot unify constructors with different arity: %s has arity %s and %s" n1 (show $ length args1) (show $ length args2)
  | otherwise =
    error $ printf "Cannot unify constructors with different names: %s and %s" n1 n2
transformUnification c@(S.C _ _) v@(S.V _) = transformUnification v c
transformUnification (S.V v) t = do
  bindVar v
  t' <- withUniqueVars [v] $ flattenInternalTerms t
  t'' <- homogenize t'
  return [Unif (Var v) t'']


flattenGoal :: (Ord a, FreshName a, Show a) => S.G a -> State (FlattenState a) (Goal a)
flattenGoal (x S.:=: y) = do
  goals <- transformUnification x y
  makeConjFromList goals
-- flattenGoal ((S.:=:) x y) = do
--   x' <- flattenTerm x
--   y' <- flattenInternalTerms y
--   m <- gets getNewVarMap
--   -- let term = fromMaybe (FTVar $ Var y') $ (Map.!?) m y'
--   makeConj (Unif (Var x') y')
flattenGoal (S.Invoke name args) = do
  args' <- withUniqueVars [] $ mapM flattenTerm args
  mapM bindVar args'
  m <- gets getVarMap
  makeConj (Call name (map Var args'))
flattenGoal (S.Conjunction x y xs) = do
  x' <- trace (printf "Flattening conjunction:\n%s \\/ %s \\/ %s" (show x) (show y) (show xs)) $
        flattenGoal x
  y' <- flattenGoal y
  xs' <- mapM flattenGoal xs
  return $ conj (Conj x' y' xs')
flattenGoal (S.Disjunction x y xs) = do
  x' <- trace (printf "Flattening disjunction:\n%s \\/ %s \\/ %s" (show x) (show y) (show xs)) $
        local flattenGoal x
  y' <- local flattenGoal y
  xs' <- mapM (local flattenGoal) xs
  return $ Disj x' y' xs'
flattenGoal (S.Fresh x g) =
  do
    modify $ \s -> s { getFreeVars = Set.insert x (getFreeVars s) }
    flattenGoal g
flattenGoal (S.Delay g) =
  EtaD <$> flattenGoal g

local :: (Ord a, FreshName a, Show a) =>
         (b -> State (FlattenState a) c) ->
         b ->
         State (FlattenState a) c
local f x = do
  oldVarMap <- gets getVarMap
  oldFreeVars <- gets getFreeVars
  r <- f x
  modify $ \s -> s { getVarMap = oldVarMap, getFreeVars = oldFreeVars }
  return r

flattenDef :: (Ord a, FreshName a, Show a) => Def S.G a -> State (FlattenState a) (Def Goal a)
flattenDef (Def name args goal) = do
  body <- flattenGoal goal
  return $ Def name args body

flattenProg :: (Ord a, FreshName a, Show a) => Program S.G a -> State (FlattenState a) (Program Goal a)
flattenProg (Program defs goal) = do
  defs <- mapM (local flattenDef) defs
  goal <- flattenGoal goal
  return $ Program defs goal

flatten :: (Ord a, FreshName a, Show a) => Program S.G a -> a -> Program Goal a
flatten program nextVar =
  evalState (flattenProg program) (initFlattenState nextVar)

flattenDefs :: (Ord a, FreshName a, Show a)
            => [Def S.G a]
            -> a
            -> [Def Goal a]
flattenDefs defs nextVar =
    evalState go (initFlattenState nextVar)
  where
    go = mapM (local flattenDef) defs

data FlattenState a = FlattenState
  { getVarSource :: a
  , getVarMap    :: Map.Map a (FlatTerm a)
  , getNewVarMap :: Map.Map a (FlatTerm a)
  , getFreeVars :: Set.Set a
  }

data UniqueVarFlattenState a = UniqueVar { usedVars :: Set.Set a, baseState :: FlattenState a }

initFlattenState :: a -> FlattenState a
initFlattenState varSource =
  FlattenState { getVarSource = varSource
               , getVarMap =  Map.empty
               , getNewVarMap = Map.empty
               , getFreeVars = Set.empty}
