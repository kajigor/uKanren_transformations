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

type Set = Set.Set
type Map = Map.Map

type Definitions = [([G S], Name, [S])]

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

generateInvokation :: [G S] -> Definitions -> Maybe (G X)
generateInvokation goals defs = do
  (goal, name, args, Just subst) <- find (\(_,_,_, subst) -> isJust subst) $
                                    map (\(g, n, args) -> (g, n, args, unifyInvokationLists g goals $ Just Eval.s0)) defs
  return $ Invoke name $ generate args subst
  where
    generate args subst =
      map (\a -> sToX $ maybe (V a) id (lookup a subst)
          )
          args

sToX :: Term S -> Term X
sToX x@(V _) = Res.vident <$> x
sToX (C n args) = C n $ map sToX args

--
-- generateCall :: [G S] -> Definitions -> G S
-- generateCall goals defs =
--   maybe (error "Call generation failed: no appropriate definition exists")
--         (\(name, argIdxs) -> Invoke name $ map (getArg goals) argIdxs
--         )
--         (getNames goals `Map.lookup` defs)
--   where
--     getArg goals (i, j) =
--       let (Invoke _ args) = goals !! i in
--       args !! j
--     getNames = map (\(Invoke name _) -> name)

-- renameGoals :: (Ord a, Show a) => [G a] -> Set Name -> (Set Name, Name, Set a)
-- renameGoals gs names =
--   let ns = map (\x -> case x of
--                         Invoke name args -> (name, args)
--                         _ -> error $ printf "Only invokations can be renamed, and you tried to rename %s" (show x)
--                ) gs in
--   let (actualName, names') = generateFreshName (humanReadableName $ map fst ns) names in
--   (names', actualName, Set.unions $ concatMap (map getVars . snd) ns)


renameGoals :: [G S] -> Definitions -> (Definitions, Name, [S])
renameGoals gs definitions =
  let ns = map (\x -> case x of
                        Invoke name args -> (name, args)
                        _ -> error $ printf "Only invokations can be renamed, and you tried to rename %s" (show x)
               ) gs in
  let actualName = generateFreshName (humanReadableName $ map fst ns) (getNames definitions) in
  let args = Set.toList $ Set.unions $ concatMap (map getVars . snd) ns in
  let newDefs = (gs, actualName, args) : definitions in
  (newDefs, actualName, args)
  where
    getNames = Set.fromList . map (\(_,x,_) -> x)

humanReadableName :: [String] -> String
humanReadableName =
  changeFirstLetter toLower . concatMap (changeFirstLetter toUpper)
  where
    changeFirstLetter f s = f (head s) : tail s

generateFreshName :: Name -> Set Name -> Name
generateFreshName n names =
  if n `notElem` names
  then n
  else until (`notElem` names) ('_' :) n


-- generateFreshName :: Name -> Set Name -> (Name, Set Name)
-- generateFreshName n names =
--   if n `notElem` names
--   then (n, Set.insert n names)
--   else
--     let name = until (`notElem` names) ('_' :) n in
--     (name, Set.insert name names)

isGroundTerm :: Term a -> Bool
isGroundTerm (V _) = False
isGroundTerm (C _ args) = all isGroundTerm args

residualizeSldTree :: [G S] -> CPD.SldTree -> Definitions -> (Def, Definitions)
residualizeSldTree rootGoals tree definitions =
  let (newDefs, defName, rootVars) = renameGoals rootGoals definitions in
  let resultants = CPD.resultants tree in
  let goals = foldl (\gs (subst, goals, _) -> go subst goals newDefs : gs) [] resultants in
  -- trace (printf "Body %s" $ show body) $
  let defArgs = map Res.vident rootVars in
  let body = Eval.postEval' defArgs $ foldl1 (|||) (reverse goals) in

  let result = (def defName defArgs body, newDefs)
  in --trace (printf "\nThis is the result of your hard work\n%s\n" (show result)) $
     result
  where
    go [] [] _       = error "Residualization failed: a substitution and goals cannot be empty simpultaneously"
    go [] gs defs    = residualizeGoals gs defs
    go subst [] defs = residualizeSubst subst
    go subst gs defs =
      let goal = residualizeGoals gs defs in
      residualizeSubst subst &&& goal

residualizeGoals :: [G S] -> Definitions -> G X
residualizeGoals gs defs =
  case generateInvokation gs defs of
    Nothing -> error $ printf "Invokation generation failed:\nno renaming for %s exists in\n%s\n" (show gs) (show defs)
    Just g -> g

-- residualizeSldTree :: [G S] -> CPD.SldTree -> Set Name -> (Def, Set Name)
-- residualizeSldTree rootGoals tree names =
--   -- let rootVars = getVars rootGoals in
--   let (ns, defName, rootVars) = renameGoals rootGoals names in
--   let resultants = CPD.resultants tree in
--   let (goals, newNames) =
--         foldl (\(gs, ns) (subst, goals, _) ->
--                   let (g, ns') = go subst goals ns in
--                   (g:gs, ns')
--               ) ([], ns) resultants
--   in
--   -- trace (printf "Body %s" $ show body) $
--   let defArgs = map Res.vident $ Set.toList rootVars in
--   let body = Eval.postEval' defArgs $ foldl1 (|||) (reverse goals) in
--
--   let result = (def defName defArgs body, newNames)
--   in trace (printf "\nThis is the result of your hard work\n%s\n" (show result)) $ result
--   where
--     go [] [] _     = error "Residualization failed: a substitution and goals cannot be empty simpultaneously"
--     go [] gs ns    = residualizeGoals gs ns
--     go subst [] ns = (residualizeSubst subst, ns)
--     go subst gs ns =
--       let (goal, newNs) = residualizeGoals gs ns in
--       (residualizeSubst subst &&& goal, newNs)

-- residualizeGoals :: [G S] -> Set Name -> (G X, Set Name)
-- residualizeGoals gs defs =
--   let (newDefs, n, args) = renameGoals gs defs in
--   (Invoke n $ map (Res.toX . V) $ Set.toList args, newDefs)

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


--
-- renameGoals :: [G a] -> State [Name] (G a)
-- renameGoals gs = do
--   let ns = map (\x -> case x of
--                         Invoke name args -> (name, args)
--                         _ -> error $ printf "Only invokations can be renamed, and you tried to rename %s" (show x)
--                ) gs
--   let newName = concatMap fst ns
--   names <- get
--
--   let actualName = if newName `notElem` names
--                    then runState (return newName) id
--                    else do
--                      let name = until (`notElem` names) ('_' :) newName
--                      runState (return name) (name:)
--
--   return $ Let (def actualName [] undefined) undefined
