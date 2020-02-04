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
import           Generalization
import           Prelude            hiding (lookup, showList)
import           Syntax
import           Text.Printf
import           Util.Miscellaneous

-- trace :: String -> a -> a
-- trace _ x = x

data Descend a = Descend { getCurr :: a, getAncs :: [a] } deriving (Eq)

instance (Show a) => Show (Descend a) where
  show (Descend curr ancs) = printf "%s <-\n%s" (show curr) (showList ancs)

type DescendGoal = Descend (G S)

data SldTree = Fail
             | Success E.Sigma
             | Or [SldTree] (Maybe (G S)) E.Sigma
             | Conj SldTree [DescendGoal] E.Sigma
             | Leaf [DescendGoal] E.Sigma E.Gamma

select :: [DescendGoal] -> Maybe DescendGoal
select = find (\x -> isSelectable embed (getCurr x) (getAncs x))

selecter :: [DescendGoal] -> ([DescendGoal], [DescendGoal])
selecter gs =
  span (\x ->
    let res = isSelectable embed (getCurr x) (getAncs x) in
    -- trace (printf "Selecter\ncurr:\n%s\nancs:%s\nrslt:%s\n" (show $ getCurr x) (showList (getAncs x)) (show res)) $
    not $ isSelectable (\x y -> embed x y || isInst x y) (getCurr x) (getAncs x)) gs
    -- not $ isSelectable embed (getCurr x) (getAncs x)) gs

-- TODO reconsider hardcoded list of basic function names
isSelectable :: Show a => (G a -> G a -> Bool) -> G a -> [G a] -> Bool
-- isSelectable _ _ ancs | Set.null ancs = True
isSelectable emb goal ancs =
  -- trace (printf "isSelectable: \nGoal: %s\nAncs: %s\n" (show goal) (show ancs)) $
  (not (any (`emb` goal) ancs) || null ancs) && fineToUnfold goal
  where
    fineToUnfold (Invoke f _) = f `notElem` basics
    fineToUnfold _            = False
    basics = [] -- ["leo", "gto"]-- [] -- ["eqNat", "eqPair"] -- ["leo", "gto"]

instance E.Subst [Descend (G S)] where
  substitute s =
    map $ \(Descend g ancs) -> Descend (E.substitute s g) ancs

sldResolution :: [G S] -> E.Gamma -> E.Sigma -> [[G S]] -> SldTree
sldResolution goal gamma subst seen  =
  -- sldResolutionStep (map (\x -> Descend x Set.empty) goal) gamma subst Set.empty True
  -- trace "\n\nSLDRESOLUTION \n\n" $
  sldResolutionStep (map (\x -> Descend x []) goal) gamma subst seen True

oneStepUnfold :: G S -> E.Gamma -> (G S, E.Gamma)
oneStepUnfold g@(Invoke f as) env@(p, i, d) =
  let (Def n fs body) = p f in
  if length fs == length as
  then
    let i' = foldl (\ interp (f, a) -> E.extend interp f a) i $ zip fs as in
    let (g', env', _) = E.preEval (p, i', d) body in
    (g', env')
  else error $ printf "Unfolding error: different number of factual and actual arguments\nFactual: %s --- %s\nActual: %s --- %s)" f (show as) n (show fs)
oneStepUnfold g env = (g, env)

showList :: Show a => [a] -> String
showList = unlines . map show

sldResolutionStep :: [DescendGoal] -> E.Gamma -> E.Sigma -> [[G S]] -> Bool -> SldTree
sldResolutionStep gs env@(p, i, d@(temp:_)) s seen isFirstTime =
  let curs = map getCurr gs in
  let prettySeen = showList seen  in
  -- if variantCheck curs seen
  if instanceCheck curs seen
  then
    Leaf gs s env
  else
    -- if temp > 13
    --   then Leaf gs s env
    --   else
        maybe (Leaf gs s env)
              (\(ls, Descend g ancs, rs) ->
                  let (g', env') = oneStepUnfold g env in
                  go g' env' ls rs g ancs isFirstTime
              )
              (selectNext gs)
      where


        selectNext gs =
          let (ls, rs) = selecter gs in
          if null rs then Nothing else Just (ls, head rs, tail rs)

        go g' env' ls rs g ancs isFirstTime =
          -- trace (printf "\nGo:\ng': %s\ng:\n%s\nls:\n%s\nrs:\n%s\n" (show g') (show g) (showList ls) (showList rs)) $
          let normalized = normalize g' in
          -- trace (printf "normalized:\n%s" $ showList normalized) $
          let unified = mapMaybe (unifyStuff s) normalized in
          -- trace (printf "unified:\n%s" $ showList unified) $
          let addDescends xs s =
                -- E.substitute s (ls ++ map (\x -> Descend x (g : ancs)) xs ++ rs) in
                E.substitute s  ( map addDescendId ls ++
                                  map (\x -> Descend x (g : ancs)) xs ++
                                  map addDescendId rs
                                )
                  where
                    addDescend goal (Descend cur ancs) = Descend goal (cur : ancs)
                    addDescendId d@(Descend cur _) = addDescend cur d
          in
          case unified of
            [] ->
              Fail
            ns | length ns == 1 || isFirstTime -> -- unfold only if it's deterministic or hasn't been unfolded before
            -- ns ->
              Or (map step ns) (Just g) s
              where
                step (xs, s') =
                  if null xs && null rs
                  then Success s'
                  else let newDescends = addDescends xs s' in
                      --  trace (printf "\n\nConj\nNew descends: %s" (show newDescends)) $
                       Conj (sldResolutionStep newDescends env' s' (map getCurr gs : seen) (isFirstTime && length ns == 1)) newDescends s'
                       -- Conj (sldResolutionStep newDescends env' s' (Set.insert (map getCurr gs) seen) False newDescends s'
            ns | not $ null rs ->
              maybe (Leaf gs s env)
                    (\(ls', Descend nextAtom nextAtomsAncs, rs')  ->
                            let (g'', env'') = oneStepUnfold nextAtom env in
                            go g'' env'' (ls ++ (Descend g ancs : ls')) rs' nextAtom nextAtomsAncs False
                    )
                    (selectNext rs)
            ns ->
              Leaf gs s env


normalize :: G S -> [[G S]] -- disjunction of conjunctions of calls and unifications
normalize (f :\/: g) = normalize f ++ normalize g
normalize (f :/\: g) = (++) <$> normalize f <*> normalize g
normalize g@(Invoke _ _) = [[g]]
normalize g@(_ :=: _) = [[g]]
normalize g = error ("Unexpected goal type in normalization\n" ++ show g)

unifyStuff :: E.Sigma -> [G S] -> Maybe ([G S], E.Sigma)
unifyStuff state gs =
    -- trace (printf "\nIn unifyStuff\nGs\n%s\nState\n%s\n" (show gs) (show state)) $
    go gs state []
  where
    go [] state conjs = Just (reverse conjs, state)
    go (g@(Invoke _ _) : gs) state conjs = go gs state (g : conjs)
    go ((t :=: u) : gs) state conjs = do
      s <- E.unify  (Just state) t u
      -- trace (printf "unifying\n%s\n" (show s)) $
      go gs s conjs

bodies :: SldTree -> [[G S]]
bodies = leaves

leaves :: SldTree -> [[G S]]
leaves (Or disjs _ _) = concatMap leaves disjs
leaves (Conj ch  _ _) = leaves ch
leaves (Leaf ds _ _)  = [map getCurr ds]
leaves _              = []

resultants :: SldTree -> [(E.Sigma, [G S], Maybe E.Gamma)]
resultants (Success s)     = [(s, [], Nothing)]
resultants (Or disjs _ _)  = concatMap resultants disjs
resultants (Conj ch _ _)   = resultants ch
resultants (Leaf ds s env) = [(s, map getCurr ds, Just env)]
resultants Fail            = []

topLevel :: Program -> SldTree
topLevel (Program defs goal) =
  -- let (goal', _, defs) = justTakeOutLets (goal, []) in
  let gamma = E.updateDefsInGamma E.env0 defs in
  let (logicGoal, gamma', names) = E.preEval gamma goal in
  sldResolutionStep [Descend logicGoal []] gamma' E.s0 [] True

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

bmc :: E.Delta -> [G S] -> [[G S]] -> ([([G S], Generalizer)], E.Delta)
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

split :: E.Delta -> [G S] -> [G S] -> (([G S], [G S]), Generalizer, E.Delta)
split d q q' = -- q <= q'
  trace (printf "splitting\nq:  %s\nq': %s\n" (show q) (show q')) $
  let n = length q in
  let qCurly = filter (\q'' -> and $ zipWith embed q q'') $ subconjs q' n in
  let (bestMC, delta) = bmc d q qCurly in
  let (b, gen) = minimallyGeneral bestMC in
  trace (printf "\nQcurly:\n%s\n\nBestMC:\n%s\n\nB:  %s\nQ': %s\nQ:  %s\n" (show' qCurly) (show' bestMC) (show b) (show q') (show q)) $
  ((b, if length q' > n then complementSubconjs b q' else []), gen, delta)
