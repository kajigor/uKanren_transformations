{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module CpdResidualization where

import Control.Monad.State
import Syntax
import Text.Printf
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Residualize as Res
import qualified CPD as CPD
import Debug.Trace
import Eval as Eval
import Data.Char
import Data.Maybe
import GlobalControl

type Set = Set.Set
type Map = Map.Map

type Definitions = [([G S], Name, [S])]

residualizeGlobalTree :: GlobalTree -> (G X -> G X)
residualizeGlobalTree tree =
  let nodes = getNodes tree in
  let definitions = foldl (\defs gs -> (\(x,_,_) -> x) (renameGoals gs defs) ) [] $ map fst nodes  in
  let lets = map (Let . fst . \(gs, sld) -> residualizeSldTree gs sld definitions) nodes in
  let result = foldl1 (.) lets in
  trace (show $ result (V "x" === V "x")) $
  result
  where
    getNodes (Leaf _ _) = []
    getNodes (Node d sld ch) = (CPD.getCurr d, sld) : (concatMap getNodes ch)

unifyInvokationLists :: [G S] -> [G S] -> Maybe Eval.Sigma -> Maybe Eval.Sigma
unifyInvokationLists [] [] state = state
unifyInvokationLists (Invoke name args : gs) (Invoke name' args' : gs') state | name == name' && length args == length args' = do
  let state' = unifyArgs args args' state
  unifyInvokationLists gs gs' state'
  where
    unifyArgs [] [] state = state
    unifyArgs (x:xs) (y:ys) state = do
      let state' = unify state x y
      unifyArgs xs ys state'
    unifyArgs _ _ _ = Nothing
unifyInvokationLists _ _ _ = Nothing

generateInvokation :: [G S] -> Definitions -> (G X, Definitions)
generateInvokation goals defs = do
  let x = find (\(_,_,_,s) -> isJust s) $
          map (\(g, n, args) -> (g, n, args, unifyInvokationLists g goals $ Just Eval.s0)) defs
  case x of
    Just (goal, name, args, Just subst) -> (Invoke name $ generate args subst, defs)
    _ -> error "oops"
         -- let (_, name, args) = renameGoals goals defs in
         -- (Invoke name $ map (sToX . V) args, defs)
  where
    generate args subst = map (\a -> sToX $ maybe (V a) id (lookup a subst)) args

sToX :: Term S -> Term X
sToX x@(V _) = Res.vident <$> x
sToX (C n args) = C n $ map sToX args

renameGoals :: [G S] -> Definitions -> (Definitions, Name, [S])
renameGoals gs definitions =
  let ns = map (\x -> case x of
                        Invoke name args -> (name, args)
                        _ -> error $ printf "Only invokations can be renamed, and you tried to rename %s" (show x)
               ) gs in
  let actualName = newName (map fst ns) definitions in
  let args = uniqueArgs $ map snd ns in
  let newDefs = (gs, actualName, args) : definitions in
  (newDefs, actualName, args)


humanReadableName :: [String] -> String
humanReadableName =
  changeFirstLetter toLower . concatMap (changeFirstLetter toUpper)
  where
    changeFirstLetter f s = f (head s) : tail s

newName :: [Name] -> Definitions -> Name
newName ns defs =
  generateFreshName (humanReadableName ns) (getNames defs)
  where
    getNames = Set.fromList . map (\(_,x,_) -> x)

uniqueArgs :: [[Term S]] -> [S]
uniqueArgs args = Set.toList $ Set.unions $ concatMap (map getVars) args

generateFreshName :: Name -> Set Name -> Name
generateFreshName n names =
  if n `notElem` names
  then n
  else until (`notElem` names) ('_' :) n

isGroundTerm :: Term a -> Bool
isGroundTerm (V _) = False
isGroundTerm (C _ args) = all isGroundTerm args

residualizeSldTree :: [G S] -> CPD.SldTree -> Definitions -> (Def, Definitions)
residualizeSldTree rootGoals tree definitions =
  let (newDefs, defName, rootVars) = renameGoals rootGoals definitions in
  let resultants = CPD.resultants tree in
  let (goals, actualDefs) =
              foldl (\(gs, defs) (subst, goals, _) ->
                       let (g, defs') = go subst goals newDefs
                       in (g : gs, defs')
                    )
                    ([], newDefs)
                    resultants in
  -- trace (printf "Body %s" $ show body) $
  let defArgs = map Res.vident rootVars in
  let body = Eval.postEval' defArgs $ foldl1 (|||) (reverse goals) in

  let result = (def defName defArgs body, actualDefs)
  in --trace (printf "\nThis is the result of your hard work\n%s\n" (show result)) $
     result
  where
    go [] [] _       = error "Residualization failed: a substitution and goals cannot be empty simpultaneously"
    go [] gs defs    = residualizeGoals gs defs
    go subst [] defs = (residualizeSubst subst, defs)
    go subst gs defs =
      let (goal, newDefs) = residualizeGoals gs defs in
      (residualizeSubst subst &&& goal, newDefs)


residualizeGoals :: [G S] -> Definitions -> (G X, Definitions)
residualizeGoals = generateInvokation

residualizeSubst subst =
  foldl1 (&&&) $ map (\(s, ts) -> Res.toX (V s) === Res.toX ts) $ reverse subst

class Ord b => UniqueVars a b | a -> b where
  getVars :: a -> Set b

instance Ord a => UniqueVars (Term a) a where
  getVars (V x) = Set.singleton x
  getVars (C _ args) = Set.unions $ map getVars args

instance Ord a => UniqueVars [Term a] a where
  getVars = Set.unions . map getVars

instance Ord a => UniqueVars (G a) a where
  getVars (Invoke _ args) = getVars args
  getVars _ = error "Get vars from anything but calls is not allowed"

instance Ord a => UniqueVars [G a] a where
  getVars = Set.unions . map getVars
