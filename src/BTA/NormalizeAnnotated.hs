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
import qualified Data.Map as Map
import           AnnotatedDef
import           AnnotatedProgram
import           AnnotationType
import qualified InvokeAnnotation as Ann
import           Text.Printf
import           BTA.SizeConversion
import Debug.Trace


goalFromList :: (AbstractG a -> AbstractG a -> [AbstractG a] -> AbstractG a) -> [AbstractG a] -> AbstractG a
goalFromList f (x : y : xs) = f x y xs
goalFromList _ [x] = x
goalFromList _ [] = error "Empty list"

unsafeConj :: [AbstractG a] -> AbstractG a
unsafeConj = goalFromList Conjunction

unsafeDisj :: [AbstractG a] -> AbstractG a
unsafeDisj = goalFromList Disjunction


data Goal a = Goal (Disj a)
            deriving (Eq,Show)

-- fresh names should be introduced on the top level of a disjunction
data Disj a = Disj [a] (NonEmpty (Conj a))
            deriving (Eq, Show)

data Conj a = Conj [a] (NonEmpty (Base a))
            deriving (Eq, Show)

data Base a = Unif (AbstractTerm a) (AbstractTerm a)
            | Call String [AbstractTerm a] Ann.Ann
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

toBase :: AbstractG String -> Either (AbstractG String) (Base String)
toBase (Invoke name args ann) = Right $ Call name args ann
toBase (t1 :=: t2) = Right $ Unif t1 t2
toBase g = Left g

norm :: AbstractG String -> State [String] [State [String] [Either (AbstractG String) (Base String)]] -- disjunction of conjunctions of calls and unifications
norm (Fresh x g) = do
  modify (x:)
  norm g
norm (Disjunction x y gs) = do
  x' <- norm x
  y' <- norm (unsafeDisj (y : gs))
  return (x' ++ y')
norm g = return [norm' g]

norm' :: AbstractG String -> State [String] [Either (AbstractG String) (Base String)]
norm' (Conjunction x y gs) = do
  x' <- norm' x
  y' <- norm' (unsafeConj (y : gs))
  return (x' ++ y')
norm' (Fresh x g) = do
  modify (x:)
  norm' g
norm' g = return [norm'' g]

norm'' :: AbstractG String -> Either (AbstractG String) (Base String)
norm'' g@(Invoke _ _ _) = toBase g
norm'' g@(_ :=: _) = toBase g
norm'' (Delay g) = toBase g
norm'' (Fresh _ g) = error "Fresh on the base level"
norm'' g = Left g

normalizeProg :: AnnotatedProgram AbstractG String -> Prg
normalizeProg (AnnotatedProgram defs goal) =
  let d = mapM (\(AnnotatedDef name args body anns) -> do
            b <- normalize (trace "normalizeProg" $ body)
            return $ Definition name args b anns
            ) defs
  in
  let (ds, state) = trace "runState" $ runState d ([],0) in
  let (g, (defs, _)) = runState (normalize goal) state in
  Prg (ds ++ defs) g

normalize :: AbstractG String -> State ([Definition], Int) (Goal String)
normalize goal = do
  let (normalized, topLevelFresh) = runState (norm goal) []
  let transformed = mapM (\ state ->
        let (bases, fresh) = runState (trace "normalize" state) [] in
        ((fresh,) <$> mapM generateNewDef bases)) normalized
  goals <- (trace "getOut" $ transformed)
  let conjs = map (\(fresh, gs) -> Conj (reverse fresh) $ fromList gs) goals
  let disj = Disj (reverse topLevelFresh) (fromList conjs)
  let result = Goal disj
  -- let result = undefined -- Goal topLevelFresh $ map (\(fresh, gs) -> Disj fresh (fromList (Conj $ fromList gs)) goals
  return result

-- normalize :: G String -> State ([Definition], Int) (Goal String)
-- normalize goal = do
--   let (normalized, topLevelFresh) = runState (norm goal) []
--   let transformed = mapM (\ state ->
--         let (bases, fresh) = runState state [] in
--         ((fresh,) <$> mapM generateNewDef bases)) normalized
--   goals <- transformed
--   let result = Goal topLevelFresh $ map  Disj [] $ fromList $ map (\g -> Conj $ fromList g) goals
--   return result

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

instance FreeVariables AbstractG Int where
  fv :: AbstractG Int -> [Int]
  fv = nub . go
    where
      go (t1 :=:  t2) = fv t1 ++ fv t2
      go (Conjunction x y gs) = concatMap go (x : y : gs)
      go (Disjunction x y gs) = concatMap go (x : y : gs)
      go (Invoke _ ts _) = concatMap fv ts
      -- go (Fresh x g)   = filter (x /=) $ go g

instance FreeVariables AbstractG String where
  fv :: AbstractG String -> [String]
  fv = nub . go
    where
      go (t1 :=: t2) = fv t1 ++ fv t2
      go (Conjunction x y gs) = concatMap go (x : y : gs)
      go (Disjunction x y gs) = concatMap go (x : y : gs)
      go (Invoke _ ts _) = concatMap fv ts
      go (Fresh x g) = filter (x /=) $ go g
      go (Delay g) = go g

generateNewDef :: Either (AbstractG String) (Base String) -> State ([Definition], Int) (Base String)
generateNewDef (Right b) = return b
generateNewDef (Left g) = do
    newName <- generateNewName
    modify (\(ds, n) -> (ds, n+1))
    body <- trace (show g) $ normalize g
    let def = Definition newName args body []
    modify (\(ds, n) -> (def:ds, n))
    return $ Call newName (map (\x -> Sum 0 (Map.fromList [("x", 1)])) args) Ann.Memo
  where
    generateNewName = do
      (defs, n) <- get
      let potentialName = printf "rel_%d" n
      return (generateFreshName potentialName (map (\(Definition n _ _ _) -> n) defs))
    args = fv g


fresh :: [a] -> AbstractG a -> AbstractG a
fresh xs g = foldr Fresh g xs

toSyntax :: Prg -> AnnotatedProgram AbstractG String
toSyntax (Prg defs g) =
    AnnotatedProgram definitions goal
  where
    goal = go g

    definitions = map (\(Definition name args g ann) -> AnnotatedDef name args (go g) ann) defs

    baseToG :: Base a -> AbstractG a
    baseToG (Unif t1 t2) = t1 :=: t2
    baseToG (Call name args ann) = Invoke name args ann

    conjToG :: Conj a -> AbstractG a
    conjToG (Conj names bases) =
      let conj = unsafeConj $ baseToG <$> toList bases in
      fresh names conj

    disjToG :: Disj a -> AbstractG a
    disjToG (Disj names conjs) =
      let disj = unsafeDisj $ conjToG <$> toList conjs in
      fresh names disj

    go (Goal disj) = disjToG disj

makeNormal :: AnnotatedProgram AbstractG String -> AnnotatedProgram AbstractG String
makeNormal = toSyntax . normalizeProg
