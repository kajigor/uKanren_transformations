{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda" #-}

module ConsPD.LocalControl where

import           Control.Monad.State
import           ConsPD.State
import           Data.List           (find, intersect, nub)
import           Data.Maybe
import qualified Data.Set            as Set
import           Descend
import           Embed
import qualified Environment         as Env
import qualified Eval                as E
import qualified FreshNames          as FN
import           Generalization
import           Prelude             hiding (lookup)
import           Program
import qualified Subst
import           Syntax
import           Text.Printf
import           Unfold              (getMaximumBranches, normalize, oneStepUnfold, unifyStuff)
import           Util.ListZipper
import qualified Util.Miscellaneous  as Util
import           Debug.Trace

data Heuristic = Deterministic | Branching deriving (Show, Read)

type DescendGoal = Descend (G S)

data SldTree = Fail
             | Success (Subst.Subst Int)
             | Or [SldTree] (Maybe (G S)) (Subst.Subst Int)
             | Conj SldTree [DescendGoal] (Subst.Subst Int)
             | Leaf [DescendGoal] (Subst.Subst Int) Env.Env
             deriving (Show)

select :: [DescendGoal] -> Maybe DescendGoal
select = find (\x -> isSelectable embed (getCurr x) (getAncs x))

selecter :: Zipper DescendGoal -> Maybe (Zipper DescendGoal)
selecter =
  goRightWhile (\x -> not $ isSelectable (\x y -> embed x y || isInst x y) (getCurr x) (getAncs x))

-- TODO reconsider hardcoded list of basic function names
isSelectable :: Show a => (G a -> G a -> Bool) -> G a -> [G a] -> Bool
isSelectable emb goal ancs =
  (not (any (`emb` goal) ancs) || null ancs) && fineToUnfold goal
  where
    fineToUnfold (Invoke f _) = f `notElem` basics
    fineToUnfold _            = False
    basics = [] -- ["leo", "gto"]-- [] -- ["eqNat", "eqPair"] -- ["leo", "gto"]

substituteDescend :: (Subst.ApplySubst a, Ord v) => Subst.Subst v -> [Descend (a v)] -> [Descend (a v)]
substituteDescend s = map $ \(Descend g ancs) -> Descend (Subst.substitute s g) ancs

sldResolution :: [G S] -> Subst.Subst S -> Heuristic -> State ConsPDState SldTree
sldResolution goal subst = sldResolutionStep (map Descend.init goal) subst True

sldResolutionStep :: [DescendGoal] -> Subst.Subst S -> Bool -> Heuristic -> State ConsPDState SldTree
sldResolutionStep gs s isFirstTime heuristic = do
  env <- gets getEnv
  seen <- gets getSeen
  let currs = map getCurr gs
  if instanceCheck currs seen
  then return $  Leaf gs s env
  else
      unfoldNext (toZipper gs) isFirstTime gs s
    where
      go g' zipper isFirstTime = do
        env' <- gets getEnv
        let d@(Descend g _) = cursor zipper
        let normalized = normalize g'
        let unified = mapMaybe (unifyStuff s) normalized
        let addDescends xs s =
              substituteDescend s
                              ( left zipper ++
                                map (`Descend.add` d) xs ++
                                right zipper
                              )
        case unified of
          [] -> return Fail
          ns | needsUnfolding heuristic env' g ns isFirstTime -> do
              ch <- mapM step ns
              return $ Or ch (Just g) s
            where
              step (xs, s') =
                if null xs && isLeftmost zipper && isRightmost zipper
                then return $ Success s'
                else do
                  let newDescends = addDescends xs s'
                  modifyEnv (const env')
                  modifySeen (Set.insert (map getCurr gs))
                  ch <- sldResolutionStep newDescends s' (isFirstTime && length ns == 1) heuristic
                  return $ Conj ch newDescends s'
          ns | not $ isRightmost zipper ->
            unfoldNext (goRight zipper) False gs s
          ns ->
            return $ Leaf gs s env'

      unfoldNext zipper isFirstTime gs s = do
        env <- gets getEnv
        case zipper >>= selecter of
          Nothing -> return $ Leaf gs s env
          Just z -> do
            let (g', env') = runState (oneStepUnfold (getCurr $ cursor z)) env
            modifyEnv (const env')
            go g' z isFirstTime

      needsUnfolding Branching env' g ns isFirstTime = getMaximumBranches env' g > length ns || isFirstTime
      needsUnfolding Deterministic _ _ ns isFirstTime = length ns == 1 || isFirstTime

bodies :: SldTree -> [[G S]]
bodies = leaves

leaves :: SldTree -> [[G S]]
leaves (Or disjs _ _) = concatMap leaves disjs
leaves (Conj ch  _ _) = leaves ch
leaves (Leaf ds _ _)  = [map getCurr ds]
leaves _              = []

resultants :: SldTree -> [(Subst.Subst Int, [G S], Maybe Env.Env)]
resultants (Success s)     = [(s, [], Nothing)]
resultants (Or disjs _ _)  = concatMap resultants disjs
resultants (Conj ch _ _)   = resultants ch
resultants (Leaf ds s env) = [(s, map getCurr ds, Just env)]
resultants Fail            = []

topLevel :: Program G X -> Heuristic -> SldTree
topLevel (Program defs goal) heuristic =
  let env = Env.fromDefs defs in
  let ((logicGoal, _), env') = runState (E.preEval goal) env in
  evalState (sldResolutionStep [Descend.init logicGoal] Subst.empty True heuristic) (ConsPD.State.init env')

mcs :: (Eq a, Show a) => [G a] -> [[G a]]
mcs []     = []
mcs [g]    = [[g]]
mcs (g:gs) =
  let (con, non, _) =
        foldl (\(con, non, vs) x -> if null (vs `intersect` vars x)
                                    then (con, x : non, vs)
                                    else (x : con, non, nub $ vars x ++ vs))
              ([g], [], vars g)
              gs
  in  reverse con : mcs (reverse non)

vars :: (Eq a, Show a) => G a -> [Term a]
vars (Invoke _ args) =
  nub $ concatMap getVars args where
    getVars (V v)    = [V v]
    getVars (C _ ts) = concatMap getVars ts
vars _ = []

msgExists gs hs | length gs == length hs =
  all (\x -> case x of (Invoke f _, Invoke g _) -> f == g; _ -> False) $ zip gs hs
msgExists _ _ = False

-- works for ordered subconjunctions
complementSubconjs :: (Instance a (Term a), Eq a, Ord a, Show a) => [G a] -> [G a] -> [G a]
complementSubconjs xs ys =
  go xs ys
   where
    go [] xs = xs
    go (x:xs) (y:ys) | x == y         = go xs ys
    go (x:xs) (y:ys) | isRenaming x y = go xs ys
    go (x:xs) (y:ys) | isInst x y     = go xs ys
    go xs (y:ys)                      = y : go xs ys
    go xs ys = error (printf "complementing %s by %s" (show xs) (show ys))

minimallyGeneral :: (Show a, Ord a) => [([G a], Generalizer)] -> ([G a], Generalizer)
minimallyGeneral xs =
  go xs xs
  where
    go [x] _     = x
    go (x:xs) ys | any (\g -> isStrictInst (fst x) (fst g)) ys = go xs ys
    go (x:xs) _  = x
    go [] _      = error "Empty list of best matching conjunctions"

bmc :: FN.FreshNames -> [G S] -> [[G S]] -> ([([G S], Generalizer)], FN.FreshNames)
bmc d q [] = ([], d)
bmc d q (q':qCurly) | msgExists q q' =
  let (generalized, _, gen, delta) = generalizeGoals d q q' in
  let (gss, delta') = bmc delta q qCurly in
  ((generalized, gen) : gss, delta')
bmc d q (q':qCurly) = bmc d q qCurly

split :: FN.FreshNames -> [G S] -> [G S] -> (([G S], [G S]), Generalizer, FN.FreshNames)
split d q q' = -- q <= q'
  let n = length q in
  let qCurly = filter (\q'' -> and $ zipWith embed q q'') $ subconjs q' n in
  let (bestMC, delta) = bmc d q qCurly in
  let (b, gen) = minimallyGeneral bestMC in
  ((b, if length q' > n then complementSubconjs b q' else []), gen, delta)