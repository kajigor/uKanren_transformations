{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Transformer.VarNormalization where

import Syntax
import Control.Monad.State (State (..), evalState, get, put, runState)
import qualified Data.Set as Set
import Text.Printf (printf)
import Data.Maybe (fromJust)

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
      g1' <- go g1
      g2' <- go g2
      gs' <- mapM go gs
      return $ Conjunction g1' g2' gs'
    go (Disjunction g1 g2 gs) = do
      g1' <- go g1
      g2' <- go g2
      gs' <- mapM go gs
      return $ Disjunction g1' g2' gs'
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

