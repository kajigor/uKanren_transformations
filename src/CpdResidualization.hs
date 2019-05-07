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
import qualified CPD
import Debug.Trace
import Eval
import Data.Char
import Data.Maybe
import GlobalControl
import SldTreePrinter
import DotPrinter
import Miscellaneous

type Set = Set.Set
type Map = Map.Map

type Definitions = [([G S], Name, [S])]

residualizationTopLevel :: GlobalTree -> G X
residualizationTopLevel test =
  let goal = residualizeGlobalTree test in
  case goal undefined of
    Let (name, args, _) _ -> goal $ Invoke name $ V <$> args
    _ -> error "Residualiation failed"

residualizeGlobalTree :: GlobalTree -> (G X -> G X)
residualizeGlobalTree tree =
  let nodes = getNodes tree in
  let definitions = foldl (\defs gs -> fst3 (renameGoals gs defs) ) [] $ map fst nodes  in
  -- trace (printf "Definitions:\n%s\n" (intercalate "\n" $ map show definitions)) $
  let lets = map Let $ mapMaybe (\(gs, sld) -> residualizeSldTree gs sld definitions) nodes in
  foldl1 (.) lets
  where
    getNodes (Leaf _ _ _) = []
    getNodes (Node d _ sld ch) = (CPD.getCurr d, sld) : (concatMap getNodes ch)

unifyInvocationLists :: [G S] -> [G S] -> Maybe Eval.Sigma -> Maybe Eval.Sigma
unifyInvocationLists [] [] state = state
unifyInvocationLists xs@(Invoke name args : gs) ys@(Invoke name' args' : gs') state | name == name' && length args == length args' = do
  let state' = trace (printf "unifying\n%s\n%s\n" (show xs) (show ys)) $  unifyArgs args args' state
  unifyInvocationLists gs gs' state'
  where
    unifyArgs [] [] state = state
    unifyArgs (x:xs) (y:ys) state = do
      let state' = trace (printf "Maybe no occurs check is not a good idea\n%s\n%s\n%s\n" (show x) (show y) (show state))  $
                   unify state x y
      unifyArgs xs ys state'
    unifyArgs _ _ _ = Nothing
    -- just unification without occurs check.
    -- unify = unifyNoOccursCheck
    unify (Just state) (V x) y | (x, y) `elem` state = Just state
    unify (Just state) (V x) y | Just (_, z) <- find ((== x) . fst) state = if y /= z then Nothing else Just state
    unify (Just state) (V x) y = Just $ (x, y) : state
    unify (Just state) (C n _) (C m _) | n /= m = Nothing
    unify (Just state) (C n xargs) (C m yargs) | n == m = unifyArgs xargs yargs (Just state)
    unify _ (C _ _) (V _) = Nothing
    unify Nothing _ _ = Nothing
unifyInvocationLists _ _ _ = Nothing

generateInvocation :: [G S] -> Definitions -> G X
generateInvocation goals defs =
  trace (printf "\nGenerateInvocation\nGoal: %s\nDefs: %s\n" (show goals) (intercalate "\n" $ map show $ map fst3 defs)) $
  fromMaybe
    (error "Residualization failed: invocation of the undefined relation.")
    (conj <$> conjInvocation goals defs)
  where
    generate args subst = map (\a -> Res.toX $ fromMaybe (V a) (lookup a subst)) args
    findDef goals defs =
      find (isJust . lst4) $
      map (\(g, n, args) -> (g, n, args, unifyInvocationLists g goals $ Just Eval.s0)) defs
    oneInvocation goals defs =
      case findDef goals defs of
        Just (goal, name, args, Just subst) -> Just $ Invoke name $ generate args subst
        _ -> Nothing
    conjInvocation [] _ = Just []
    conjInvocation goals defs =
      trace (printf "invocation for\n%s\n" $ show goals) $
      let representable =
            filter isJust $
            map (divideInvocations defs) $
            concatMap (generateSplits goals) $
            reverse [1 .. length goals] in
      case representable of
        (x : _) -> x
        _ -> Nothing

    divideInvocations defs (cur, rest) =
      case oneInvocation cur defs of
        Just x -> (x :) <$> conjInvocation rest defs
        Nothing -> Nothing


renameGoals :: [G S] -> Definitions -> (Definitions, Name, [S])
renameGoals gs definitions =
  let ns = map (\x -> case x of
                        Invoke name args -> (name, args)
                        _ -> error $ printf "Only invocations can be renamed, and you tried to rename %s" (show x)
               ) gs in
  let actualName = newName (map fst ns) definitions in
  let args = uniqueArgs $ map snd ns in
  let newDefs = (gs, actualName, args) : definitions in
  (newDefs, actualName, args)


humanReadableName :: [String] -> String
humanReadableName =
  map (\x -> if x == '\'' then '_' else x) .
  changeFirstLetter toLower .
  concatMap (changeFirstLetter toUpper)
  where
    changeFirstLetter f s = f (head s) : tail s

newName :: [Name] -> Definitions -> Name
newName ns defs =
  generateFreshName (humanReadableName ns) (getNames defs)
  where
    getNames = Set.fromList . map snd3

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

residualizeSldTree :: [G S] -> CPD.SldTree -> Definitions -> Maybe Def
residualizeSldTree rootGoals tree definitions = do
  let (_, defName, rootVars) = fromMaybe (error (printf "Residualization failed: no definition found for\n%s\nDefs:\n%s\n" (show rootGoals) (show definitions))) $
                               find ((== rootGoals) . fst3) definitions
  let resultants = CPD.resultants tree
  let goals = foldl (\gs (subst, goals, _) ->
                       let g = go subst goals definitions
                       in  g : gs
                    )
                    []
                    resultants
  let defArgs = map Res.vident rootVars

  let body = Eval.postEval' defArgs $ foldl1 (|||) (reverse goals)

  if null goals
  then fail (printf "No resultants in the sld tree for %s" (show rootGoals))
  else return $ def defName defArgs body
  where
    go [] [] defs    = error "Residualization failed: a substitution and goals cannot be empty simpultaneously"
    go [] gs defs    = residualizeGoals gs defs
    go subst [] defs = residualizeSubst subst
    go subst gs defs =
      let goal = residualizeGoals gs defs in
      residualizeSubst subst &&& goal

conj = foldl1 (&&&)

residualizeGoals :: [G S] -> Definitions -> G X
residualizeGoals = generateInvocation

residualizeSubst :: Eval.Sigma -> G X
residualizeSubst subst =
  conj $ map (\(s, ts) -> Res.toX (V s) === Res.toX ts) $ reverse subst

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
