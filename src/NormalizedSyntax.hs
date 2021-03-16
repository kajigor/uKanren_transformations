{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module NormalizedSyntax where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Syntax
import Text.Printf
import Control.Monad.State
import Data.List.NonEmpty.Extra (fromList)
import Data.List (intercalate)
import Debug.Trace (trace)


data Goal a = Goal (Disj a)
            deriving (Eq, Ord, Functor, Show)

-- fresh names should be introduced on the top level of a disjunction
data Disj a = Disj [Name] (NonEmpty (Conj a))
            deriving (Eq, Ord, Functor, Show)

data Conj a = Conj [Name] (NonEmpty (Base a))
            deriving (Eq, Ord, Functor, Show)

data Base a = Unif (Term a) (Term a)
            | Call Name [Term a]
            deriving (Eq, Ord, Functor, Show)


-- Definitions
data Definition = Definition Name [Name] (Goal X)
                deriving (Eq, Ord)

instance Show Definition where
  show (Definition name args body) = printf "%s %s = %s" name (unwords args) (show body)

data Prg = Prg [Definition] (Goal X)
          deriving (Eq)

instance Show Prg where
  show (Prg defs goal) =
    let shownDefs = intercalate "\n\n" $ map show defs in
    let shownGoal = show goal in
    printf "%s\n\n? %s" shownDefs shownGoal

toBase :: G X -> Either (G X) (Base X)
toBase (Invoke name args) = Right $ Call name args
toBase (t1 :=: t2) = Right $ Unif t1 t2
toBase g = Left g

norm :: G X -> State [Name] [State [Name] [Either (G X) (Base X)]] -- disjunction of conjunctions of calls and unifications
norm (Fresh x g) = do
  modify (x:)
  norm g
norm (Conjunction x y gs) = do
  x' <- norm x
  y' <- norm (unsafeConj (y : gs))
  return (x' ++ y')
norm g = return [norm' g]

norm' :: G X -> State [Name] [Either (G X) (Base X)]
norm' (Disjunction x y gs) = do
  x' <- norm' x
  y' <- norm' (unsafeDisj (y : gs))
  return (x' ++ y')
norm' (Fresh x g) = do
  modify (x:)
  norm' g
norm' g = return [norm'' g]
norm'' :: G X -> Either (G X) (Base X)
norm'' g@(Invoke _ _) = toBase g
norm'' g@(_ :=: _) = toBase g
-- !!!
norm'' (Fresh _ g) = error "Fresh on the base level"
norm'' g = Left g

normalizeProg :: Program -> Prg
normalizeProg (Program defs goal) =
  let d = mapM (\(Def name args body) -> do
            b <- normalize body
            return $ Definition name args b
            ) defs
  in
  let (ds, state) = runState d ([],0) in
  let (g, (defs, _)) = runState (normalize goal) state in
  Prg (ds ++ defs) g

normalize :: G X -> State ([Definition], Int) (Goal X)
normalize goal = do
  let (normalized, topLevelFresh) = runState (norm goal) []
  let transformed = mapM (\ state ->
        let (bases, fresh) = runState state [] in
        ((fresh,) <$> mapM generateNewDef bases)) normalized
  goals <- transformed
  let conjs = map (\(fresh, gs) -> Conj (reverse fresh) $ fromList gs) goals
  let disj = Disj (reverse topLevelFresh) (fromList conjs)
  let result = Goal disj
  -- let result = undefined -- Goal topLevelFresh $ map (\(fresh, gs) -> Disj fresh (fromList (Conj $ fromList gs)) goals
  return result

-- normalize :: G X -> State ([Definition], Int) (Goal X)
-- normalize goal = do
--   let (normalized, topLevelFresh) = runState (norm goal) []
--   let transformed = mapM (\ state ->
--         let (bases, fresh) = runState state [] in
--         ((fresh,) <$> mapM generateNewDef bases)) normalized
--   goals <- transformed
--   let result = Goal topLevelFresh $ map  Disj [] $ fromList $ map (\g -> Conj $ fromList g) goals
--   return result

generateFreshName :: Name -> [Name] -> Name
generateFreshName n names =
  if n `notElem` names
  then n
  else until (`notElem` names) ('_' :) n

generateNewDef :: Either (G X) (Base X) -> State ([Definition], Int) (Base X)
generateNewDef (Right b) = return b
generateNewDef (Left g) = do
    newName <- generateNewName
    modify (\(ds, n) -> (ds, n+1))
    body <- normalize g
    let def = Definition newName args body
    modify (\(ds, n) -> (def:ds, n))
    return $ Call newName (map V args)
  where
    generateNewName = do
      (defs, n) <- get
      let potentialName = printf "rel_%d" n
      return (generateFreshName potentialName (map (\(Definition n _ _) -> n) defs))
    args = fvg g


toSyntax :: Prg -> Program
toSyntax (Prg defs g) =
    Program definitions goal
  where
    goal = go g

    definitions = map (\(Definition name args g) -> Def name args (go g)) defs

    baseToG :: Base a -> G a
    baseToG (Unif t1 t2) = t1 :=: t2
    baseToG (Call name args) = Invoke name args

    conjToG :: Conj a -> G a
    conjToG (Conj names bases) =
      let conj = unsafeConj $ baseToG <$> toList bases in
      fresh names conj

    disjToG :: Disj a -> G a
    disjToG (Disj names conjs) =
      let disj = unsafeConj $ conjToG <$> toList conjs in
      fresh names disj

    go (Goal disj) = disjToG disj

makeNormal :: Program -> Program
makeNormal = toSyntax . normalizeProg