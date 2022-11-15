{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module CPD.Residualization where

import Syntax
import Def
import Program
import Text.Printf
import Data.List
import qualified Data.Set as Set
import qualified Residualization as Res
import qualified CPD.LocalControl as LC
import Eval
import Data.Char
import Data.Maybe
import CPD.GlobalControl
import Util.Miscellaneous
import qualified Subst

type Set = Set.Set

type Definitions = [([G S], Name, [S])]

residualizationTopLevel :: GlobalTree -> Program G X
residualizationTopLevel test =
  case residualizeGlobalTree test of
    defs@(Def name args _ : t) -> Program defs (Invoke name $ V <$> args)
    _ -> error "Residualiation failed: no defs generated"

residualizeGlobalTree :: GlobalTree -> [Def G X]
residualizeGlobalTree tree =
  let nodes = getNodes tree in
  let definitions = foldl (\defs gs -> fst3 (renameGoals gs defs) ) [] $ map fst nodes  in
  mapMaybe (\(gs, sld) -> residualizeSldTree gs sld definitions) nodes

unifyInvocationLists :: [G S] -> [G S] -> Maybe Subst.Subst -> Maybe Subst.Subst
unifyInvocationLists [] [] subst = subst
unifyInvocationLists xs@(Invoke name args : gs) ys@(Invoke name' args' : gs') subst | name == name' && length args == length args' = do
  let subst' = unifyArgs args args' subst
  unifyInvocationLists gs gs' subst'
  where
    unifyArgs [] [] subst = subst
    unifyArgs (x:xs) (y:ys) subst = do
      let subst' = unify subst x y
      unifyArgs xs ys subst'
    unifyArgs _ _ _ = Nothing
    -- just unification without occurs check.
    -- unify = unifyNoOccursCheck
    unify (Just subst) (V x) y =
      case Subst.lookup x subst of
        Just z | z == y -> Just subst
        Just z -> Nothing
        Nothing -> Just $ Subst.insert x y subst
    -- unify (Just subst) (V x) y | (x, y) `elem` subst = Just subst
    -- unify (Just subst) (V x) y | Just (_, z) <- find ((== x) . fst) subst = if y /= z then Nothing else Just subst
    -- unify (Just subst) (V x) y = Just $ (x, y) : subst
    unify (Just subst) (C n _) (C m _) | n /= m = Nothing
    unify (Just subst) (C n xargs) (C m yargs) | n == m = unifyArgs xargs yargs (Just subst)
    unify _ (C _ _) (V _) = Nothing
    unify Nothing _ _ = Nothing
unifyInvocationLists _ _ _ = Nothing

generateInvocation :: [G S] -> Definitions -> G X
generateInvocation goals defs = do
  fromMaybe
    (error "Residualization failed: invocation of the undefined relation.")
    (conj =<< conjInvocation goals defs)
  where
    generate args subst = map (\a -> Res.toX $ fromMaybe (V a) (Subst.lookup a subst)) args
    findDef goals defs =
      find (isJust . lst4) $
      map (\(g, n, args) -> (g, n, args, unifyInvocationLists g goals $ Just Subst.empty)) defs
    oneInvocation goals defs =
      case findDef goals defs of
        Just (goal, name, args, Just subst) -> Just $ Invoke name $ generate args subst
        _ -> Nothing
    conjInvocation [] _ = Just []
    conjInvocation goals defs =
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

residualizeSldTree :: [G S] -> LC.SldTree -> Definitions -> Maybe (Def G X)
residualizeSldTree rootGoals tree definitions = do
  let (_, defName, rootVars) = fromMaybe (error (printf "Residualization failed: no definition found for\n%s\nDefs:\n%s\n" (show rootGoals) (show definitions))) $
                               find ((== rootGoals) . fst3) definitions

  let resultants = LC.resultants tree

  let goals = foldl (\gs (subst, goals, _) ->
                       let g = go subst goals definitions
                       in  g : gs
                    )
                    []
                    resultants
  let defArgs = Res.vident <$> rootVars
  let body = Eval.postEval defArgs $
                unsafeDisj (reverse goals)

  if null goals
  then fail (printf "No resultants in the sld tree for %s" (show rootGoals))
  else return $ Def defName defArgs body
  where
    go subst [] defs | Subst.null subst = success
    go subst gs defs | Subst.null subst = residualizeGoals gs defs
    go subst [] defs = residualizeSubst subst
    go subst gs defs =
      let goal = residualizeGoals gs defs in
      residualizeSubst subst &&& goal

residualizeGoals :: [G S] -> Definitions -> G X
residualizeGoals = generateInvocation

residualizeSubst :: Subst.Subst -> G X
residualizeSubst subst =
  unsafeConj $ map (\(s, ts) -> Res.toX (V s) === Res.toX ts) $ reverse (Subst.toList subst)

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
