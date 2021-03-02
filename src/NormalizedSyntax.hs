{-# LANGUAGE DeriveFunctor #-}
module NormalizedSyntax where

import Data.List.NonEmpty (NonEmpty (..))
import Syntax
import Text.Printf
import Control.Monad.State
import Data.List.NonEmpty.Extra (fromList)
import Data.List (intercalate)

data Goal a = Goal (Disj a)
            deriving (Eq, Ord, Functor, Show)

-- fresh names should be introduced on the top level of a disjunction
data Disj a = Disj [Name] (NonEmpty (Conj a))
            deriving (Eq, Ord, Functor, Show)

data Conj a = Conj (NonEmpty (Base a))
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

norm :: G X -> [[Either (G X) (Base X)]] -- disjunction of conjunctions of calls and unifications
norm (f :\/: g) = norm f ++ norm g
norm g = [norm' g]
norm' :: G X -> [Either (G X) (Base X)]
norm' (f :/\: g) = norm' f ++ norm' g
norm' (Fresh _ g) = norm' g
norm' g = [norm'' g]
norm'' :: G X -> Either (G X) (Base X)
norm'' g@(Invoke _ _) = toBase g
norm'' g@(_ :=: _) = toBase g
-- !!!
norm'' (Fresh _ g) = norm'' g
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
  let normalized = norm goal
  let transformed = mapM (mapM generateNewDef) normalized
  goals <- transformed
  let result = Goal $ Disj [] $ fromList $ map (\g -> Conj $ fromList g) goals
  return result

generateFreshName :: Name -> [Name] -> Name
generateFreshName n names =
  if n `notElem` names
  then n
  else until (`notElem` names) ('_' :) n

generateNewDef :: Either (G X) (Base X) -> State ([Definition], Int) (Base X)
generateNewDef (Right b) = return b
generateNewDef (Left g) = do
    newName <- generateNewName
    body <- normalize g
    let def = Definition newName args body
    modify (\(ds, n) -> (def:ds, n+1))
    return $ Call newName (map V args)
  where
    generateNewName = do
      (defs, n) <- get
      let potentialName = printf "rel_%d" n
      return (generateFreshName potentialName (map (\(Definition n _ _) -> n) defs))
    args = fvg g
