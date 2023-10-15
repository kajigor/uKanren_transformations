{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BTA.NormalizeAnnotated where

import           Control.Monad.State
import           Data.List                (intercalate, nub)
import           Data.List.NonEmpty       (NonEmpty (..), toList)
import           Data.List.NonEmpty.Extra (fromList)
import           Debug.Trace
import           Text.Printf
import           BTA.AnnotatedDef
import           BTA.AnnotatedProgram
import           BTA.AnnotationType
import           BTA.InvokeAnnotation
import           BTA.SizeConversion
import qualified Data.Map as Map


goalFromList :: (AnnG t a -> AnnG t a -> [AnnG t a] -> AnnG t a) -> [AnnG t a] -> AnnG t a
goalFromList f (x : y : xs) = f x y xs
goalFromList _ [x] = x
goalFromList _ [] = error "Empty list"

unsafeConj :: [AnnG t a] -> AnnG t a
unsafeConj = goalFromList Conjunction

unsafeDisj :: [AnnG t a] -> AnnG t a
unsafeDisj = goalFromList Disjunction


data Goal a = Goal (Disj a)
            deriving (Eq,Show)

-- fresh names should be introduced on the top level of a disjunction
data Disj a = Disj [a] (NonEmpty (Conj a))
            deriving (Eq, Show)

data Conj a = Conj [a] (NonEmpty (Base a))
            deriving (Eq, Show)

data Base a = Unif (AbstractTerm a) (AbstractTerm a)
            | Call String [AbstractTerm a] Ann
            deriving (Eq, Show)

-- Definitions
data Definition = Definition String [String] (Goal String) [AnnotationType]
                deriving (Eq)

instance Show Definition where
  show (Definition name args body anns) = printf "filter (%s) \n %s %s = %s" (unwords $ map show anns) name (unwords args) (show body)

data Prg = Prg [Definition] (Goal String)
          deriving (Eq)

instance Show Prg where
  show (Prg defs goal) =
    let shownDefs = intercalate "\n\n" $ map show defs in
    let shownGoal = show goal in
    printf "%s\n\n? %s" shownDefs shownGoal

toBase :: AnnG AbstractTerm String -> Either (AnnG AbstractTerm String) (Base String)
toBase (Invoke name args ann) = Right $ Call name args ann
toBase (t1 :=: t2) = Right $ Unif t1 t2
toBase g = Left g

norm :: AnnG AbstractTerm String -> State [String] [State [String] [Either (AnnG AbstractTerm String) (Base String)]] -- disjunction of conjunctions of calls and unifications
norm (Fresh x g) = do
  modify (x:)
  norm g
norm (Disjunction x y gs) = do
  x' <- norm x
  y' <- norm (unsafeDisj (y : gs))
  return (x' ++ y')
norm g = return [norm' g]

norm' :: AnnG AbstractTerm String -> State [String] [Either (AnnG AbstractTerm String) (Base String)]
norm' (Conjunction x y gs) = do
  x' <- norm' x
  y' <- norm' (unsafeConj (y : gs))
  return (x' ++ y')
norm' (Fresh x g) = do
  modify (x:)
  norm' g
norm' g = return [norm'' g]

norm'' :: AnnG AbstractTerm String -> Either (AnnG AbstractTerm String) (Base String)
norm'' g@(Invoke _ _ _) = toBase g
norm'' g@(_ :=: _) = toBase g
norm'' (Delay g) = toBase g
norm'' (Fresh _ g) = error "Fresh on the base level"
norm'' g = Left g

normalizeProg :: AnnotatedProgram (AnnG AbstractTerm) String -> Prg
normalizeProg (AnnotatedProgram defs goal) =
  let d = mapM (\(AnnotatedDef name args body anns) -> do
            b <- normalize body
            return $ Definition name args b anns
            ) defs
  in
  let (ds, state) = runState d ([],0) in
  let (g, (defs, _)) = runState (normalize goal) state in
  Prg (ds ++ defs) g

normalize :: AnnG AbstractTerm String -> State ([Definition], Int) (Goal String)
normalize goal = do
  let (normalized, topLevelFresh) = runState (norm goal) []
  let transformed = mapM (\ state ->
        let (bases, fresh) = runState (state) [] in
        ((fresh,) <$> mapM generateNewDef bases)) normalized
  goals <- (transformed)
  let conjs = map (\(fresh, gs) -> Conj (reverse fresh) $ fromList gs) goals
  let disj = Disj (reverse topLevelFresh) (fromList conjs)
  let result = Goal disj
  return result

generateFreshName :: String -> [String] -> String
generateFreshName n names =
  if n `notElem` names
  then n
  else until (`notElem` names) ('_' :) n


class Eq a => FreeVariables t a where
  fv :: t a -> [a]

instance Eq a => FreeVariables AbstractTerm a where
  fv :: Eq a => AbstractTerm a -> [a]
  fv = nub . go
    where
      go (Sum n mp)    = Map.keys mp

instance FreeVariables (AnnG AbstractTerm) Int where
  fv :: AnnG AbstractTerm Int -> [Int]
  fv = nub . go
    where
      go (t1 :=:  t2) = fv t1 ++ fv t2
      go (Conjunction x y gs) = concatMap go (x : y : gs)
      go (Disjunction x y gs) = concatMap go (x : y : gs)
      go (Invoke _ ts _) = concatMap fv ts

instance FreeVariables (AnnG AbstractTerm) String where
  fv :: AnnG AbstractTerm String -> [String]
  fv = nub . go
    where
      go (t1 :=: t2) = fv t1 ++ fv t2
      go (Conjunction x y gs) = concatMap go (x : y : gs)
      go (Disjunction x y gs) = concatMap go (x : y : gs)
      go (Invoke _ ts _) = concatMap fv ts
      go (Fresh x g) = filter (x /=) $ go g
      go (Delay g) = go g

generateNewDef :: Either (AnnG AbstractTerm String) (Base String) -> State ([Definition], Int) (Base String)
generateNewDef (Right b) = return b
generateNewDef (Left g) = do
    newName <- generateNewName
    modify (\(ds, n) -> (ds, n+1))
    body <- normalize g
    let def = Definition newName args body $ replicate (length args) Dynamic
    modify (\(ds, n) -> (def:ds, n))
    return $ Call newName (map (\x -> Sum 0 (Map.fromList [(x, 1)])) args) Unfold
  where
    generateNewName = do
      (defs, n) <- get
      let potentialName = printf "rel_%d" n
      return (generateFreshName potentialName (map (\(Definition n _ _ _) -> n) defs))
    args = fv g


fresh :: [a] -> AnnG termType a -> AnnG termType a
fresh xs g = foldr Fresh g xs

toSyntax :: Prg -> AnnotatedProgram (AnnG AbstractTerm) String
toSyntax (Prg defs g) =
    AnnotatedProgram definitions goal
  where
    goal = go g

    definitions = map (\(Definition name args g ann) -> AnnotatedDef name args (go g) ann) defs

    baseToG :: Base a -> AnnG AbstractTerm a
    baseToG (Unif t1 t2) = t1 :=: t2
    baseToG (Call name args ann) = Invoke name args ann

    conjToG :: Conj a -> AnnG AbstractTerm a
    conjToG (Conj names bases) =
      let conj = unsafeConj $ baseToG <$> toList bases in
      fresh names conj

    disjToG :: Disj a -> AnnG AbstractTerm a
    disjToG (Disj names conjs) =
      let disj = unsafeDisj $ conjToG <$> toList conjs in
      fresh names disj

    go (Goal disj) = disjToG disj

makeNormal :: AnnotatedProgram (AnnG AbstractTerm) String -> AnnotatedProgram (AnnG AbstractTerm) String
makeNormal = toSyntax . normalizeProg
