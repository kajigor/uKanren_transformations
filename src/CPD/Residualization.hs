{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module CPD.Residualization where

import           CPD.GlobalControl
import qualified CPD.LocalControl   as LC
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Set           as Set
import qualified Data.Map.Strict    as Map
import           Def
import           Eval
import           Program
import qualified Residualization    as Res
import qualified Subst
import           Syntax
import           Text.Printf
import           Util.Miscellaneous
import Debug.Trace 

type Set = Set.Set

type Definitions = [([G S], Name, [S])]

residualizationTopLevel :: GlobalTree -> Program G X
residualizationTopLevel = residualizationTopLevelWith Map.empty

-- | Residualization using the recorded abstraction of every resultant (see
--   'CPD.GlobalControl.topLevelAbstr'): a resultant is residualized as the
--   conjunction of calls to the definitions of its pieces, instantiated by
--   the pieces' generalizers.  Without a record, the partition is re-derived
--   by matching (which cannot express generalizations that break variable
--   sharing between pieces).
residualizationTopLevelWith :: Abstr -> GlobalTree -> Program G X
residualizationTopLevelWith = residualizationTopLevelWithOrig []

-- | With the original definitions: a piece that has no residual definition
--   (possible once a budget stopped specialization) calls a copy of the
--   original relation, @orig_p@; the copies that are needed are appended.
residualizationTopLevelWithOrig :: [Def G X] -> Abstr -> GlobalTree -> Program G X
residualizationTopLevelWithOrig orig abstr test =
  case residualizeGlobalTree' (not (null orig)) abstr test of
    defs@(Def name args _ : t) -> Program (defs ++ origCopies orig defs) (Invoke name $ V <$> args)
    _ -> trace "residualization failed" $ Program [] (C "" [] === C "a" []) -- error "Residualiation failed: no defs generated"

type Abstr = Map.Map ([G S], [G S]) [([G S], Subst.Subst S)]

origPrefix :: String
origPrefix = "orig_"

-- | Copies of the original relations reachable from calls to @orig_*@.
origCopies :: [Def G X] -> [Def G X] -> [Def G X]
origCopies orig defs =
  let om = [ (getName d, d) | d <- orig ]
      callsOf g = case g of
        Invoke f _ -> [f]
        Conjunction a b cs -> concatMap callsOf (a : b : cs)
        Disjunction a b cs -> concatMap callsOf (a : b : cs)
        Fresh _ h -> callsOf h
        Delay h -> callsOf h
        _ -> []
      wanted = [ drop (length origPrefix) f | d <- defs, f <- callsOf (getBody d), origPrefix `isPrefixOf` f ]
      close seen [] = seen
      close seen (f : fs) | f `elem` seen = close seen fs
                          | otherwise = case lookup f om of
                              Just d -> close (f : seen) (callsOf (getBody d) ++ fs)
                              Nothing -> close seen fs
      ren g = case g of
        Invoke f ts | f `elem` map fst om -> Invoke (origPrefix ++ f) ts
        Conjunction a b cs -> unsafeConj (map ren (a : b : cs))
        Disjunction a b cs -> unsafeDisj (map ren (a : b : cs))
        Fresh x h -> Fresh x (ren h)
        Delay h -> Delay (ren h)
        _ -> g
  in [ Def (origPrefix ++ n) as (ren b) | n <- reverse (close [] wanted), Just (Def _ as b) <- [lookup n om] ]

residualizeGlobalTree :: Abstr -> GlobalTree -> [Def G X]
residualizeGlobalTree = residualizeGlobalTree' False

residualizeGlobalTree' :: Bool -> Abstr -> GlobalTree -> [Def G X]
residualizeGlobalTree' useOrig abstr tree =
  let nodes = getNodes tree in
  let definitions = foldl (\defs gs -> fst3 (renameGoals gs defs) ) [] $ map fst nodes  in
  mapMaybe (\(gs, sld) -> residualizeSldTree' useOrig abstr gs sld definitions) nodes

unifyInvocationLists :: [G S] -> [G S] -> Maybe (Subst.Subst S) -> Maybe (Subst.Subst S)
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
    (error $ printf "Residualization failed: invocation of an undefined relation.\nGoals: %s\nDefs:\n%s\n" (show goals) (intercalate "\n" $ map show defs)) 
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
    -- Partition the conjunction into pieces, each an instance of a
    -- definition's goal list (as an ordered subsequence).  The piece that
    -- contains the first goal is chosen first, longest definitions first;
    -- only goals with matching relation names are tried, which replaces the
    -- previous enumeration of all subsequences (exponential: it made
    -- residualization of queens5 take minutes).
    conjInvocation [] _ = Just []
    conjInvocation (g0 : rest) defs =
      firstJust
        [ (Invoke name (generate args subst) :) <$> conjInvocation remaining defs
        | (pat@(p0 : ps), name, args) <- sortOn (negate . length . fst3) defs
        , length pat <= 1 + length rest
        , sameRel p0 g0
        , (chosen, remaining) <- orderedMatches ps rest
        , Just subst <- [unifyInvocationLists pat (g0 : chosen) (Just Subst.empty)]
        ]
    orderedMatches [] rest = [([], rest)]
    orderedMatches (p : ps) rest =
      [ (x : xs, before ++ rem)
      | (before, x : after) <- [ splitAt i rest | i <- [0 .. length rest - 1] ]
      , sameRel p x
      , (xs, rem) <- orderedMatches ps after ]
    sameRel (Invoke f as) (Invoke g bs) = f == g && length as == length bs
    sameRel _ _ = False
    firstJust xs = case [ x | Just x <- xs ] of
      (x : _) -> Just x
      [] -> Nothing


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

residualizeSldTree :: Abstr -> [G S] -> LC.SldTree -> Definitions -> Maybe (Def G X)
residualizeSldTree = residualizeSldTree' False

residualizeSldTree' :: Bool -> Abstr -> [G S] -> LC.SldTree -> Definitions -> Maybe (Def G X)
residualizeSldTree' useOrig abstr rootGoals tree definitions = do
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

  -- A node without resultants is a goal that fails: residualize it as an
  -- explicit failing definition, so that the residual program stays closed
  -- (every called relation is defined).
  if null goals
  then return $ Def defName defArgs failure
  else return $ Def defName defArgs body
  where
    go subst [] defs | Subst.null subst = success
    go subst gs defs | Subst.null subst = invoke gs defs
    go subst [] defs = residualizeSubst subst
    go subst gs defs =
      let goal = invoke gs defs in
      residualizeSubst subst &&& goal
    invoke gs defs = case Map.lookup (rootGoals, gs) abstr of
      Just pieces | Just inv <- mapM (\(p, gen) -> pieceOrOrig p gen defs) pieces ->
        case concat inv of
          []  -> success
          [x] -> x
          (x : y : zs) -> Conjunction x y zs
      _ -> residualizeGoals gs defs
    pieceOrOrig p gen defs = case pieceInvocation p gen defs of
      Just i -> Just [i]
      Nothing | useOrig -> Just [ Invoke (origPrefix ++ f) [ Res.toX (Subst.substitute gen t) | t <- ts ]
                                | Invoke f ts <- p ]
      Nothing -> Nothing

-- | Call of the definition of an (abstracted) piece, instantiated by the
--   piece's generalizer.
pieceInvocation :: [G S] -> Subst.Subst S -> Definitions -> Maybe (G X)
pieceInvocation [] _ _ = Just success
pieceInvocation piece gen defs =
  case [ (n, args, s) | (g, n, args) <- defs, Just s <- [unifyInvocationLists g piece (Just Subst.empty)] ] of
    ((n, args, s) : _) ->
      Just $ Invoke n [ Res.toX (Subst.substitute gen (fromMaybe (V a) (Subst.lookup a s))) | a <- args ]
    [] -> Nothing

residualizeGoals :: [G S] -> Definitions -> G X
residualizeGoals = generateInvocation

residualizeSubst :: Subst.Subst S -> G X
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