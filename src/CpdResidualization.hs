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

residualizeGlobalTree :: GlobalTree -> (G X -> G X)
residualizeGlobalTree tree =
  let nodes = getNodes tree in
  let definitions = foldl (\defs gs -> fst3 (renameGoals gs defs) ) [] $ map fst nodes  in
  -- trace (printf "Definitions:\n%s\n" (intercalate "\n" $ map show definitions)) $
  let lets = map (Let . \(gs, sld) -> -- trace (printf "\n\n\n%s\n\n\n" $ simplyPrintTree sld) $
                                      residualizeSldTree gs sld definitions) nodes in
  foldl1 (.) lets
  where
    getNodes (Leaf _ _ _) = []
    getNodes (Node d _ sld ch) = (CPD.getCurr d, sld) : (concatMap getNodes ch)

unifyInvocationLists :: [G S] -> [G S] -> Maybe Eval.Sigma -> Maybe Eval.Sigma
unifyInvocationLists [] [] state = state
unifyInvocationLists (Invoke name args : gs) (Invoke name' args' : gs') state | name == name' && length args == length args' = do
  let state' = unifyArgs args args' state
  unifyInvocationLists gs gs' state'
  where
    unifyArgs [] [] state = state
    unifyArgs (x:xs) (y:ys) state = do
      let state' = unify__ state x y
      unifyArgs xs ys state'
    unifyArgs _ _ _ = Nothing

    -- just unification without occurs check. TODO get rid of the code duplication
    unify__ Nothing _ _ = Nothing
    unify__ st@(Just subst) u v =
      unify' (walk u subst) (walk v subst)  where
        unify' (V u') (V v') | u' == v' = Just subst
        unify' (V u') t = Just $ (u', v) : subst
        unify' t (V v') = Just $ (v', u) : subst
        unify' (C a as) (C b bs) | a == b && length as == length bs =
          foldl (\ st' (u', v') -> unify__ st' u' v') st $ zip as bs
        unify' _ _ = Nothing
        walk x@(V v') s =
          case lookup v' s of
            Nothing -> x
            Just t  -> walk t s
        walk u' _ = u'


unifyInvocationLists _ _ _ = Nothing

generateInvocation :: [G S] -> Definitions -> G X
generateInvocation goals defs =
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
      -- trace (printf "invocation for\n%s\n" $ show goals) $
      let representable =
            filter isJust $
            map (divideInvocations defs) $
            concatMap (generateSplits goals) $ reverse [1 .. length goals] in
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
  changeFirstLetter toLower . concatMap (changeFirstLetter toUpper)
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

residualizeSldTree :: [G S] -> CPD.SldTree -> Definitions -> Def
residualizeSldTree rootGoals tree definitions =
  let (_, defName, rootVars) = fromMaybe (error "Residualization failed: no definition found") $
                               find ((== rootGoals) . fst3) definitions in
  let resultants = CPD.resultants tree in
  let goals = foldl (\gs (subst, goals, _) ->
                       let g = go subst goals definitions
                       in  g : gs
                    )
                    []
                    resultants in
  let defArgs = map Res.vident rootVars in
  let body = Eval.postEval' defArgs $ foldl1 (|||) (reverse goals) in

  let result = def defName defArgs body
  in --trace (printf "\nThis is the result of your hard work\n%s\n" (show result)) $
     result
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
