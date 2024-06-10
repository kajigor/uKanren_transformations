{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module OfflinePD.LocalControl where

import           Control.Monad.State
import           Data.Maybe
import           Data.List (intersect, nub)
import           BTA.InvokeAnnotation
import           OfflinePD.AnnUnfold
import           Syntax (Term(..), S)
import           Descend 
import           Util.ListZipper
import           Text.Printf                      (printf)
import qualified OfflinePD.AnnEnvironment as Env
import qualified FreshNames as FN
import qualified Subst           
import           Embed           
import           Generalization (Generalizer)
import           OfflinePD.Generalization    
import qualified CPD.LocalControl as LCcpd

import           Debug.Trace
import           CPD.LocalControl (Heuristic)

type DescendGoal = Descend (AnnG Term S)

data SldTree = Fail
             | Success (Subst.Subst S)
             | Or [SldTree] (Maybe (AnnG Term S)) (Subst.Subst S)
             | Conj SldTree [DescendGoal] (Subst.Subst S)
             | Leaf [DescendGoal] (Subst.Subst S) Env.Env

conjToList :: AnnG Term a -> [AnnG Term a]
conjToList (Conjunction x y gs) = x : y : gs
conjToList x = [x]

selecter :: Zipper DescendGoal -> Maybe (Zipper DescendGoal)
selecter =
  goRightWhile (\x -> not $ isSelectable (\x y -> embed (convertToSimple x) (convertToSimple y) || isInst (convertToSimple x) (convertToSimple y)) (getCurr x) (getAncs x))
  

isSelectable :: Show a =>  (AnnG Term a -> AnnG Term a -> Bool) -> AnnG Term a -> [AnnG Term a] -> Bool
isSelectable _ (Invoke _ _ Unfold) _ = True
isSelectable emb goal@Invoke {} ancs = not (any (`emb` goal) ancs) || null ancs --trace (show goal ++ " " ++ show ancs ++ show (not (any (`emb` goal) ancs))) ancs))
isSelectable _ _ _ = False


sldResolution :: [AnnG Term S] -> Env.Env -> Subst.Subst S -> [[AnnG Term S]] -> LCcpd.Heuristic -> SldTree
sldResolution goal = sldResolutionStep (map (`Descend` []) goal) True

sldResolutionStep :: [DescendGoal] -> Bool -> Env.Env -> Subst.Subst S -> [[AnnG Term S]] -> LCcpd.Heuristic -> SldTree
sldResolutionStep gs isFirstTime env s seen heu =
  let (temp, _) = FN.getFreshName (Env.getFreshNames env) in
  let curs = map getCurr -- $ trace ("Local: " ++ show gs) -- $ traceShow seen 
              gs in
  if instanceCheck curs seen
  then
    Leaf gs s -- $ trace "instanceCheck" 
              env
  else
    unfoldNext (toZipper gs) isFirstTime gs s env
  where
    go :: AnnG Term S -> Env.Env -> Zipper (Descend (AnnG Term S)) -> Bool -> SldTree  
    go g' env' zipper isFirstTime =
      let Descend g ancs = cursor -- $ traceShow g' 
                                    zipper in
      let normalized = normalize g' in
      let unified = mapMaybe (unifyStuff s) normalized in
      let addDescends xs s =
            LCcpd.substituteDescend s
                            ( left zipper ++
                              map (\x -> Descend x (g : ancs)) xs ++
                              right zipper
                            )
      in
      case unified of
        [] -> Fail
        ns | needsUnfolding heu env' g ns isFirstTime ->
            Or (map step ns) (Just g) s
          where
            step (xs, s') =
              if null xs && isLeftmost zipper && isRightmost zipper
              then Success s'
              else let newDescends = addDescends xs s' in
                   Conj (sldResolutionStep newDescends (isFirstTime && length ns == 1) env' s' (map getCurr gs : seen) heu) newDescends s'
        ns | not $ isRightmost zipper ->
          unfoldNext (goRight zipper) False gs s env
        ns ->
          Leaf gs s -- $ trace "unified" 
            env
                     

    unfoldNext :: Maybe (Zipper (Descend (AnnG Term S))) -> Bool -> [DescendGoal] -> Subst.Subst S -> Env.Env -> SldTree 
    unfoldNext zipper isFirstTime gs s env =
      maybe (Leaf gs s env)
            (\z ->
                let (g', env') = runState (oneStepUnfold (getCurr $ cursor z)) env in -- $ trace (show (getCurr $ cursor z) ++ " " ++ show (length seen) ++ " " ++ show seen)
                go g' env' z isFirstTime
            )
            (zipper >>= selecter)
            
    needsUnfolding _ _ (Invoke _ _ Unfold) _ _ = True
    needsUnfolding LCcpd.Branching env' g ns isFirstTime = getMaximumBranches env' g > length ns || isFirstTime
    needsUnfolding LCcpd.Deterministic _ _ ns isFirstTime = length ns == 1 || isFirstTime

bodies :: SldTree -> [[AnnG Term S]]
bodies = leaves

leaves :: SldTree -> [[AnnG Term S]]
leaves (Or disjs _ _) = concatMap leaves disjs
leaves (Conj ch  _ _) = leaves ch
leaves (Leaf ds _ _)  = [map getCurr ds]
leaves _              = []

resultants :: SldTree -> [(Subst.Subst S, [AnnG Term S], Maybe Env.Env)]
resultants (Success s)     = [(s, [], Nothing)]
resultants (Or disjs _ _)  = concatMap resultants disjs
resultants (Conj ch _ _)   = resultants ch
resultants (Leaf ds s env) = [(s, map getCurr ds, Just env)]
resultants Fail            = []


mcs :: (Eq a, Show a) => [AnnG Term a] -> [[AnnG Term a]]
mcs []     = []
mcs [g]    = [[g]]
mcs (g:gs) =
  let (con, non, _) =
        foldl (\(con, non, vs) x -> if null (vs `intersect` vars x)
                                    then (con, x : non, vs)
                                    else (x : con, non, nub $ vars x ++ vs))
              ([g], [], vars g)
              -- $ trace ("mcs" ++ show gs) 
              gs
  in  reverse con : mcs ( --trace (show con ++ show non) 
                          reverse non)

vars :: (Eq a, Show a) => AnnG Term a -> [Term a]
vars (Invoke _ args _) =
  nub $ concatMap getVars args where
    getVars (V v)    = [V v]
    getVars (C _ ts) = concatMap getVars ts
vars _ = []

msgExists gs hs | length gs == length hs =
  all (\x -> case x of (Invoke f _ _, Invoke g _ _) -> f == g; _ -> False) $ zip gs hs
msgExists _ _ = False

-- works for ordered subconjunctions
complementSubconjs :: (Instance a (Term a), Eq a, Ord a, Show a) => [AnnG Term a] -> [AnnG Term a] -> [AnnG Term a]
complementSubconjs xs ys =
  go xs ys
   where
    go [] xs = xs
    go (x:xs) (y:ys) | x == y         = go xs ys
    go (x:xs) (y:ys) | isRenaming x y = go xs ys
    go (x:xs) (y:ys) | isInst x y     = go xs ys
    -- go (x:xs) (y:ys) | isInst y x     = go xs ys
    go xs (y:ys)                  = y : go xs ys
    go xs ys = error (printf "complementing %s by %s" (show xs) (show ys))

-- TODO : implemented literally according to the definition, may be inefficient. Look at the graph approach again.
-- elem q is minimally general of Q iff there doesn't exist another elem q' \in Q which is a strict instance (q' = q \Theta)
-- isStrictInst q t iff q = t \Theta
minimallyGeneral :: (Show a, Ord a) => [([AnnG Term a], Generalizer)] -> ([AnnG Term a], Generalizer)
minimallyGeneral xs =
  go xs xs
  where
    go [x] _     = x
    go (x:xs) ys | any (\g -> isStrictInst (fst x) (fst g)) ys = go xs ys
    go (x:xs) _  = x
    go [] _      = error "Empty list of best matching conjunctions"

bmc :: FN.FreshNames -> [AnnG Term S] -> [[AnnG Term S]] -> ([([AnnG Term S], Generalizer)], FN.FreshNames)
bmc d q [] = ([], d)
bmc d q (q':qCurly) | msgExists q q' =
  let (generalized, _, gen, delta) = generalizeGoals d q q' in
  let (gss, delta') = bmc delta q qCurly in
  ((generalized, gen) : gss, delta')
bmc d q (q':qCurly) = bmc d q qCurly

split :: FN.FreshNames -> [AnnG Term S] -> [AnnG Term S] -> (([AnnG Term S], [AnnG Term S]), Generalizer, FN.FreshNames)
split d q q' = -- q <= q'
  let n = length q in
  let qCurly = filter (\q'' -> and $ zipWith embed q q'') $ subconjs q' n in
  let (bestMC, delta) = bmc d q qCurly in
  let (b, gen) = minimallyGeneral bestMC in
  ((b, if length q' > n then complementSubconjs b q' else []), gen, delta)

