{-# LANGUAGE DeriveFunctor #-}
module Mode.NormSyntax where

import Mode.Term
import Data.List.NonEmpty (NonEmpty (..))
import qualified Mode.Syntax as S
import Control.Monad.State
import Program
import Def
import Data.List (intersect)
import qualified Data.Set as Set
import Data.Bifunctor (second)

data Base a = Unif (Var a) (FlatTerm a)
            | Call String [Var a]
            deriving (Show, Eq, Functor)

newtype Conj a = Conj (NonEmpty (Base a))
               deriving (Show, Eq, Functor)

newtype Disj a = Disj (NonEmpty (Conj a))
               deriving (Show, Eq, Functor)

type Goal = Disj

data NameSource = NameSource
  { usedNames :: [String]
  , counter :: Int
  }

newNameSource :: [String] -> NameSource
newNameSource xs = NameSource { usedNames = xs, counter = 0 }

normalize :: (Ord a, Eq a, Show a) => Program S.Goal a -> Program Goal a
normalize program =
    let namesTaken = map getName (getDefs program) in
    evalState (normalizeProgram program) (newNameSource namesTaken, Set.empty)
  where
    normalizeProgram program = do
      defs <- mapM normalizeDef (getDefs program)
      goal <- normalizeGoal (getGoal program) "topLevel" (S.allVars $ getGoal program)
      newDefs <- normalizeNewDefs
      return $ Program (defs ++ newDefs) goal

    normalizeDef def = do
      body <- normalizeGoal (getBody def) (getName def) (getArgs def)
      return $ def { getBody = body }


    normalizeNewDefs :: (Ord a, Eq a) => State (NameSource, Set.Set (Def S.Goal a)) [Def Goal a]
    normalizeNewDefs = do
      defs <- gets snd
      if Set.null defs
      then
        return []
      else do
        let (def, rest) = Set.deleteFindMin defs
        modify (second (const rest))
        def <- normalizeDef def
        (def :) <$> normalizeNewDefs

    normalizeGoal :: (Ord a, Eq a) => S.Goal a -> String -> [a] -> State (NameSource, Set.Set (Def S.Goal a)) (Goal a)
    normalizeGoal goal name args =
        goDisj goal
      where
        newName :: State (NameSource, a) String
        newName = do
          (nameSupply, x) <- get
          let n = name ++ show (counter nameSupply)
          if n `elem` usedNames nameSupply
          then do
            put (nameSupply { counter = counter nameSupply + 1 }, x)
            newName
          else
            return n

        newCall goal = do
          n <- newName
          let vars = S.allVars goal `intersect` args
          let newDef = Def { getName = n, getArgs = vars, getBody = goal }
          modify (second (Set.insert newDef))
          return $ Call n $ map Var vars
        goDisj (S.Disj x y xs) = do
          x <- goConj x
          y <- goConj y
          xs <- mapM goConj xs
          return $ Disj $ x :| (y : xs)
        goDisj goal = do
          goal <- goConj goal
          return $ Disj $ goal :| []
        goConj (S.Conj x y xs) = do
          x <- goBase x
          y <- goBase y
          xs <- mapM goBase xs
          return $ Conj $ x :| (y : xs)
        goConj goal@(S.Disj _ _ _) = do
          call <- newCall goal
          return $ Conj $ call :| []
        goConj goal = do
          goal <- goBase goal
          return $ Conj $ goal :| []
        goBase (S.Call n as) =
          return $ Call n as
        goBase (S.Unif x t) =
          return $ Unif x t
        goBase goal =
          newCall goal

back :: Program Goal a -> Program S.Goal a
back (Program defs goal) =
    Program (map backDef defs) (backGoal goal)
  where
    backDef (Def name args body) =
      Def name args (backGoal goal)
    backGoal (Disj (x :| [])) = backConj x
    backGoal (Disj (x :| (h:t))) = S.Disj (backConj x) (backConj h) (map backConj t)
    backConj (Conj (x :| [])) = backBase x
    backConj (Conj (x :| (h:t))) = S.Conj (backBase x) (backBase h) (map backBase t)
    backBase (Unif x t) = S.Unif x t
    backBase (Call name args) = S.Call name args

