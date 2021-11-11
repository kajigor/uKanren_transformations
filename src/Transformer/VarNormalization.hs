{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Transformer.VarNormalization where

import Syntax
import Control.Monad.State (State (..), evalState, get, put, modify, runState)
import Control.Monad.Reader ( Reader, runReader, ask )
import qualified Data.Set as Set
import Text.Printf (printf)
import Data.Maybe (fromJust, isJust)
import Data.List (nub)

data AnnotatedVar a = RelArgument a | FreshVar a

varToString :: Show a => AnnotatedVar a -> X
varToString (RelArgument x) = printf "x.%s" $ show x
varToString (FreshVar x) = printf "q.%s" $ show x

uniqueVarGoal :: G X -> [X] -> (G X, [X])
uniqueVarGoal goal boundArgs =
    let varMap = [(v, varToString (RelArgument i)) | (i, v) <- zip [0..] boundArgs] in
    let goal' = evalState (go goal) (varMap, 0) in
    (goal', map snd varMap)
  where
    renameVar varMap x =
      fromJust $ lookup x varMap
    go :: G X -> State ([(X, X)], Int) (G X)
    go (t1 :=: t2) = do
      (varMap, _) <- get
      return $ (renameVar varMap <$> t1) :=: (renameVar varMap <$> t2)
    go (Conjunction g1 g2 gs) = do
      g1 <- go g1
      g2 <- go g2
      gs <- mapM go gs
      return $ Conjunction g1 g2 gs
    go (Disjunction g1 g2 gs) = do
      g1 <- go g1
      g2 <- go g2
      gs <- mapM go gs
      return $ Disjunction g1 g2 gs
    go (Invoke f fs) = do
      (varMap, _) <- get
      let fs' = map (renameVar varMap <$>) fs
      return $ Invoke f fs'
    go (Fresh x g) = do
      (varMap, next) <- get
      let x' = varToString (FreshVar next)
      put ((x, x') : varMap, next + 1)
      Fresh x' <$> go g

uniqueVarRename :: Program -> Program
uniqueVarRename (Program defs goal) =
    let defs' = map uniqueDef defs in
    let goal' = uniqueGoal goal in
    Program defs' goal'
  where
    uniqueDef (Def name args goal) =
      let (goal', args') = uniqueVarGoal goal args in
      Def name args' goal'
    uniqueGoal goal =
      fst $ uniqueVarGoal goal []

allVarsDefined :: Program -> Bool
allVarsDefined (Program defs goal) =
    and (checkGoal goal [] : [checkDef def | def <- defs])
  where
    checkGoal goal scope =
        evalState (go goal) scope
      where
        go :: G X -> State [X] Bool
        go (t1 :=: t2) = do
          scope <- get
          let vars1 = getVars t1
          let vars2 = getVars t2
          return $ all (`elem` scope) (vars1 ++ vars2)
        go (Invoke _ args) = do
          scope <- get
          let vars = concatMap getVars args
          return $ all (`elem` scope) vars
        go (Conjunction g1 g2 gs) = do
          scope <- get
          return $ and [ evalState (go g) scope | g <- g1 : g2 : gs]
        go (Disjunction g1 g2 gs) = do
          scope <- get
          return $ and [ evalState (go g) scope | g <- g1 : g2 : gs]
        go (Fresh v g) = do
          modify (v:)
          go g
    checkDef (Def _ args goal) =
      checkGoal goal args

    getVars  =
        nub . go
      where
        go (V x) = [x]
        go (C _ xs) = concatMap go xs

allRelationsDefined :: Program -> Bool
allRelationsDefined (Program defs goal) =
    let signatures = [ (name, length args) | Def name args _ <- defs ] in
    and (checkGoal goal signatures : [checkDef def signatures | def <- defs])
  where
    checkGoal goal signatures =
        runReader (go goal) signatures
      where
        go :: G X -> Reader [(Name, Int)] Bool
        -- TODO: get rid of this case, make success goal representable and not just assumed to be defined
        go (Invoke successName []) = return True
        go (t1 :=: t2) = return True
        go (Invoke name args) = do
          signatures <- ask
          return $ (name, length args) `elem` signatures
        go (Conjunction g1 g2 gs) = do
          and <$> mapM go (g1 : g2 : gs)
        go (Disjunction g1 g2 gs) = do
          and <$> mapM go (g1 : g2 : gs)
        go (Fresh v g) = go g
    checkDef (Def _ _ goal) =
      checkGoal goal

normalize :: Program -> Program
normalize (Program defs goal) =
    Program (map normalizeDef defs) (normalizeGoal goal)
  where
    normalizeDef :: Def -> Def
    normalizeDef (Def name args goal) = Def name args (normalizeGoal goal)

    normalizeGoal :: G X -> G X
    normalizeGoal goal =
        evalState (go goal) Set.empty
      where
        go :: G X -> State (Set.Set Name) (G X)
        go goal = do
          names <- get
          -- case goal of
          --   te :=: te' -> _
          --   Conjunction g g' gs -> _
          --   Disjunction g g' gs -> _
          --   Fresh s g -> _
          --   Invoke name args -> _
          undefined

