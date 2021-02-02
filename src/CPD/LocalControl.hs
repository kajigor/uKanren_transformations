{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module CPD.LocalControl where

import           Data.List          (find, intersect, nub)
import           Data.Maybe
import           Debug.Trace
import           Embed
import qualified Eval               as E
import qualified FreshNames         as FN
import           Generalization
import           Prelude            hiding (lookup)
import qualified Subst
import           Syntax
import           Text.Printf
import           Unfold             (oneStepUnfold, normalize, unifyStuff, getMaximumBranches)
import qualified Environment as Env
import           Util.ListZipper
import qualified Util.Miscellaneous as Util
import           Descend

-- trace :: String -> a -> a
-- trace _ x = x

data Heuristic = Deterministic | Branching


type DescendGoal = Descend (G S)

data SldTree = Fail
             | Success Subst.Subst
             | Or [SldTree] (Maybe (G S)) Subst.Subst
             | Conj SldTree [DescendGoal] Subst.Subst
             | Leaf [DescendGoal] Subst.Subst Env.Env

select :: [DescendGoal] -> Maybe DescendGoal
select = find (\x -> isSelectable embed (getCurr x) (getAncs x))

selecter :: Zipper DescendGoal -> Maybe (Zipper DescendGoal)
selecter =
  goRightWhile (\x -> not $ isSelectable (\x y -> embed x y || isInst x y) (getCurr x) (getAncs x))
    -- not $ isSelectable embed (getCurr x) (getAncs x)) gs

-- TODO reconsider hardcoded list of basic function names
isSelectable :: Show a => (G a -> G a -> Bool) -> G a -> [G a] -> Bool
-- isSelectable _ _ ancs | Set.null ancs = True
isSelectable emb goal ancs =
  (not (any (`emb` goal) ancs) || null ancs) && fineToUnfold goal
  where
    fineToUnfold (Invoke f _) = f `notElem` basics
    fineToUnfold _            = False
    basics = [] -- ["leo", "gto"]-- [] -- ["eqNat", "eqPair"] -- ["leo", "gto"]

instance Subst.ApplySubst [Descend (G S)] where
  substitute s =
    map $ \(Descend g ancs) -> Descend (Subst.substitute s g) ancs

sldResolution :: [G S] -> Env.Env -> Subst.Subst -> [[G S]] -> Heuristic -> SldTree
sldResolution goal env subst seen =
  -- sldResolutionStep (map (\x -> Descend x Set.empty) goal) env subst Set.empty True
  -- trace "\n\nSLDRESOLUTION \n\n" $
  sldResolutionStep (map (\x -> Descend x []) goal) env subst seen True

sldResolutionStep :: [DescendGoal] -> Env.Env -> Subst.Subst -> [[G S]] -> Bool -> Heuristic -> SldTree
sldResolutionStep gs env s seen isFirstTime heuristic =
  trace (printf "Local control\n%s\n\n" (show gs))  $
  let (temp, _) = FN.getFreshName (Env.getFreshNames env) in
  let curs = map getCurr gs in
  let prettySeen = Util.showList "" seen  in
  -- if variantCheck curs seen
  if instanceCheck curs seen
  then
    Leaf gs s env
  else
    -- if temp > 13
    --   then Leaf gs s env
    --   else
        maybe (Leaf gs s env)
              (\(Zipper (ls, Descend g ancs, rs)) ->
                  let (g', env') = oneStepUnfold g env in
                  go g' env' ls rs g ancs isFirstTime
              )
              (selectNext gs)
      where

        selectNext :: [DescendGoal] -> Maybe (Zipper DescendGoal)
        selectNext gs = do
          z <- toZipper gs
          traceM (printf "Zipper: %s\n" $ show z)
          let r = selecter z
          traceM (printf "Selecter: %s\n" $ show r)
          r

        go g' env' ls rs g ancs isFirstTime =
          let normalized = normalize g' in
          let unified = mapMaybe (unifyStuff s) normalized in
          let addDescends xs s =
                -- Subst.substitute s (ls ++ map (\x -> Descend x (g : ancs)) xs ++ rs) in
                Subst.substitute s  (reverse ls ++
                                  map (\x -> Descend x (g : ancs)) xs ++
                                  rs
                                )
                  where
                    addDescend goal (Descend cur ancs) = Descend goal (cur : ancs)
                    addDescendId d@(Descend cur _) = addDescend cur d
          in
          case unified of
            [] ->
              Fail
            -- ns | length ns == 1 || isFirstTime -> -- unfold only if it's deterministic or hasn't been unfolded before
            ns | needsUnfolding heuristic env' g ns isFirstTime -> -- getMaximumBranches env' g > length ns || isFirstTime ->
              Or (map step ns) (Just g) s
              where
                step (xs, s') =
                  if null xs && null rs && null ls
                  then Success s'
                  else let newDescends = addDescends xs s' in
                       Conj (sldResolutionStep newDescends env' s' (map getCurr gs : seen) (isFirstTime && length ns == 1) heuristic) newDescends s'
                       -- Conj (sldResolutionStep newDescends env' s' (Set.insert (map getCurr gs) seen) False newDescends s'
            ns | not $ null rs ->
              maybe (Leaf gs s env)
                    (\(Zipper (ls', Descend nextAtom nextAtomsAncs, rs'))  ->
                            let (g'', env'') = oneStepUnfold nextAtom env in
                            go g'' env'' (reverse ls ++ (Descend g ancs : reverse ls')) rs' nextAtom nextAtomsAncs False
                    )
                    (selectNext rs)
            ns ->
              Leaf gs s env

        needsUnfolding Branching env' g ns isFirstTime = getMaximumBranches env' g > length ns || isFirstTime
        needsUnfolding Deterministic _ _ ns isFirstTime = length ns == 1 || isFirstTime

bodies :: SldTree -> [[G S]]
bodies = leaves

leaves :: SldTree -> [[G S]]
leaves (Or disjs _ _) = concatMap leaves disjs
leaves (Conj ch  _ _) = leaves ch
leaves (Leaf ds _ _)  = [map getCurr ds]
leaves _              = []

resultants :: SldTree -> [(Subst.Subst, [G S], Maybe Env.Env)]
resultants (Success s)     = [(s, [], Nothing)]
resultants (Or disjs _ _)  = concatMap resultants disjs
resultants (Conj ch _ _)   = resultants ch
resultants (Leaf ds s env) = [(s, map getCurr ds, Just env)]
resultants Fail            = []

topLevel :: Program -> Heuristic -> SldTree
topLevel (Program defs goal) heuristic =
  let env = Env.fromDefs defs in
  let (logicGoal, env', _) = E.preEval env goal in
  sldResolutionStep [Descend logicGoal []] env' Subst.empty [] True heuristic

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
  -- trace (printf "\nComplementing\n%s\nby\n%s\n" (show xs) (show ys)) $
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
minimallyGeneral :: (Show a, Ord a) => [([G a], Generalizer)] -> ([G a], Generalizer)
minimallyGeneral xs =
  -- trace (printf "minimallyGeneral %s" $ show xs) $
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
  -- trace (printf "Generalizing\nq:   %s\nq':  %s\nRes: %s\nGen: %s\ndelta: %s\n" (show q) (show q') (show generalized) (show gen) (show $ head d)) $
  let (gss, delta') = bmc delta q qCurly in
  ((generalized, gen) : gss, delta')
  -- if head d >= 50
  -- then error "ooops"
  -- else
  --   trace (printf "Generalizing\nq:   %s\nq':  %s\nRes: %s\nGen: %s\ndelta: %s\n" (show q) (show q') (show generalized) (show gen) (show $ head d)) $
  --   let (gss, delta') = bmc delta q qCurly in
  --   ((generalized, gen) : gss, delta')
bmc d q (q':qCurly) = trace "why msg does not exist?!" $ bmc d q qCurly
-- bmc d q qCurly = [(\(x,_,_,_) -> x) $ D.generalizeGoals d q q' | q' <- qCurly, msgExists q q']

split :: FN.FreshNames -> [G S] -> [G S] -> (([G S], [G S]), Generalizer, FN.FreshNames)
split d q q' = -- q <= q'
  -- trace (printf "splitting\nq:  %s\nq': %s\n" (show q) (show q')) $
  let n = length q in
  let qCurly = filter (\q'' -> and $ zipWith embed q q'') $ subconjs q' n in
  let (bestMC, delta) = bmc d q qCurly in
  let (b, gen) = minimallyGeneral bestMC in
  -- trace (printf "\nQcurly:\n%s\n\nBestMC:\n%s\n\nB:  %s\nQ': %s\nQ:  %s\n" (show' qCurly) (show' bestMC) (show b) (show q') (show q)) $
  ((b, if length q' > n then complementSubconjs b q' else []), gen, delta)
