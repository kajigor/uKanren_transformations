{-# LANGUAGE DeriveFunctor #-}
module Mode.Syntax where

import           Control.Monad.State
import           Data.List           (nub)
import qualified Data.Map.Strict     as Map
import           Def
import           FreshNames
import           Mode.Term
import           Program
import qualified Syntax              as S

data Goal a = Call String [Var a]
            | Unif (Var a) (FlatTerm a)
            | Conj (Goal a) (Goal a) [Goal a]
            | Disj (Goal a) (Goal a) [Goal a]
            | EtaD (Goal a)
            deriving (Show, Eq, Ord, Functor)

allVars :: Eq a => Goal a -> [a]
allVars =
    nub . map getVar . go
  where
    go (Call _ args) = args
    go (Unif v t) = v : varsFromTerm t
    go (Conj x y xs) = go x ++ go y ++ concatMap go xs
    go (Disj x y xs) = go x ++ go y ++ concatMap go xs
    go (EtaD g) = go g

freshVar :: FreshName a => State (FlattenState a) a
freshVar = do
  state <- get
  let (v', rest) = getFreshName (FreshNames (getVarSource state))
  put $ state { getVarSource = unFreshNames rest }
  return v'

addTerm :: Ord a => a -> FlatTerm a -> State (FlattenState a) ()
addTerm key value = do
  state <- get
  put $ state { getVarMap = Map.insert key value (getVarMap state)
              , getNewVarMap = Map.insert key value (getNewVarMap state)
              }

flattenTerm :: (Ord a, FreshName a) => S.Term a -> State (FlattenState a) a
flattenTerm (S.V v) = return v
flattenTerm (S.C name args) = do
  newVar <- freshVar
  args' <- mapM flattenTerm args
  addTerm newVar (FTCon name (map Var args'))
  return newVar

flattenInternalTerms :: (Ord a, FreshName a) => S.Term a -> State (FlattenState a) (FlatTerm a)
flattenInternalTerms (S.V v) = return $ FTVar $ Var v
flattenInternalTerms (S.C name args) = do
  args' <- mapM flattenTerm args
  return $ FTCon name $ map Var args'

conj :: Goal a -> Goal a -> [Goal a] -> Goal a
conj (Conj x y xs) (Conj x' y' xs') rest = Conj x y (xs ++ x' : y' : xs' ++ rest)
conj g (Conj x y xs) rest = Conj g x (y : xs ++ rest)
conj x y rest = Conj x y rest

disj :: Goal a -> Goal a -> [Goal a] -> Goal a
disj (Disj x y xs) (Disj x' y' xs') rest = Disj x y (xs ++ x' : y' : xs' ++ rest)
disj g (Disj x y xs) rest = Disj g x (y : xs ++ rest)
disj x y rest = Disj x y rest

toConj :: Goal a -> [Goal a] -> Goal a
toConj x [] = x
toConj x (h:t) = Conj x h t

makeConj :: Goal a -> State (FlattenState a) (Goal a)
makeConj term = do
  state <- get
  put $ state { getNewVarMap = Map.empty }
  return $ toConj term $ map (\(x, t) -> Unif (Var x) t) $ Map.toList (getNewVarMap state)

flattenGoal :: (Ord a, FreshName a, Show a) => S.G a -> State (FlattenState a) (Goal a)
flattenGoal ((S.:=:) x y) = do
  x' <- flattenTerm x
  y' <- flattenInternalTerms y
  m <- gets getNewVarMap
  -- let term = fromMaybe (FTVar $ Var y') $ (Map.!?) m y'
  makeConj (Unif (Var x') y')
flattenGoal (S.Invoke name args) = do
  args' <- mapM flattenTerm args
  m <- gets getVarMap
  makeConj (Call name (map Var args'))
flattenGoal (S.Conjunction x y xs) = do
  x' <- flattenGoal x
  y' <- flattenGoal y
  xs' <- mapM flattenGoal xs
  return $ conj x' y' xs'
flattenGoal (S.Disjunction x y xs) = do
  x' <- local flattenGoal x
  y' <- local flattenGoal y
  xs' <- mapM (local flattenGoal) xs
  return $ disj x' y' xs'
flattenGoal (S.Fresh _ g) =
  flattenGoal g
flattenGoal (S.Delay g) =
  EtaD <$> flattenGoal g

local :: (Ord a, FreshName a, Show a) =>
         (b -> State (FlattenState a) c) ->
         b ->
         State (FlattenState a) c
local f x = do
  oldVarMap <- gets getVarMap
  r <- f x
  state <- get
  put $ state { getVarMap = oldVarMap }
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

data FlattenState a = FlattenState
  { getVarSource :: a
  , getVarMap    :: Map.Map a (FlatTerm a)
  , getNewVarMap :: Map.Map a (FlatTerm a)
  }

initFlattenState :: a -> FlattenState a
initFlattenState varSource =
  FlattenState { getVarSource = varSource
               , getVarMap =  Map.empty
               , getNewVarMap = Map.empty}
