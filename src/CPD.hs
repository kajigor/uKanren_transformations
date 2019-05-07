
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module CPD where

import Prelude hiding (lookup)
import Syntax
import qualified Eval as E
import Text.Printf
import Control.Monad
import Data.Maybe
import Data.List (find, nub, intersect, partition, subsequences)
import Purification
import qualified Data.Map.Strict as Map
import Debug.Trace
import Data.List
import qualified Driving as D
import qualified Data.Set as Set

data Descend a = Descend { getCurr :: a, getAncs :: Set a } deriving (Eq)

instance (Show a) => Show (Descend a) where
  show (Descend curr ancs) = printf "%s <- %s" (show curr) (show $ Set.toList ancs)

type DescendGoal = Descend (G S)

data SldTree = Fail
             | Success E.Sigma
             | Or [SldTree] E.Sigma
             | Conj SldTree [DescendGoal] E.Sigma
             | Leaf [DescendGoal] E.Sigma E.Gamma

select :: [DescendGoal] -> Maybe DescendGoal
select = find (\x -> isSelectable embed (getCurr x) (getAncs x))

selecter :: [DescendGoal] -> ([DescendGoal], [DescendGoal])
selecter gs = span (\x -> not $ isSelectable embed (getCurr x) (getAncs x)) gs

-- TODO reconsider hardcoded list of basic function names
isSelectable :: Show a => (G a -> G a -> Bool) -> G a -> Set (G a) -> Bool
isSelectable _ _ ancs | Set.null ancs = True
isSelectable emb goal ancs =
  (not $ any (`emb` goal) ancs) && fineToUnfold goal
  where
    fineToUnfold (Invoke f _) = f `notElem` basics
    fineToUnfold _ = False
    basics = [] -- ["eqNat", "eqPair"] -- ["leo", "gto"]

substituteDescend s =
  map $ \(Descend g ancs) -> Descend (E.substituteGoal s g) ancs

sldResolution :: [G S] -> E.Gamma -> E.Sigma -> SldTree
sldResolution goal gamma subst =
  sldResolutionStep (map (\x -> Descend x Set.empty) goal) gamma subst Set.empty True

sldResolutionStep :: [DescendGoal] -> E.Gamma -> E.Sigma -> Set [G S] -> Bool -> SldTree
sldResolutionStep gs env@(p, i, d@(temp:_)) s seen isFirstTime =
  -- let false = C "false" [] in
  -- let isFalseCheck x =
  --       case x of
  --         Invoke "all_check_uni" args -> last args == false
  --         Invoke "check_uni" args -> last args == false
  --         _ -> False in
  -- let gs' = filter isFalseCheck $ map getCurr gs in
  -- trace (printf "\nResolution step:\ngs: \n%s" $ intercalate "\n" (map show gs)) $
  if variantCheck (map getCurr gs) seen
  then Leaf gs s env
  else
    -- if temp > 100
      -- then Leaf gs s env
      -- else
        maybe (Leaf gs s env)
              (\(ls, Descend g ancs, rs) ->
                  -- trace (printf "\nSelected: %s\nAncs: %s" (show g) (show $ Set.toList ancs)) $
                  let (g', env') = unfold g env in
                  go g' env' ls rs g ancs isFirstTime
              )
              (selectNext gs)
      where
        unfold g@(Invoke f as) env@(p, i, d)  =
          let (n, fs, body) = p f in
          if length fs == length as
          then
            let i' = foldl (\ interp (f, a) -> E.extend interp f a) i $ zip fs as in
            let (g', env', _) = E.preEval' (p, i', d) body in
            (g', env')
          else error $ printf "Unfolding error: different number of factual and actual arguments\nFactual: %s --- %s\nActual: %s --- %s)" f (show as) n (show fs)
        unfold g env = (g, env)

        selectNext gs =
          let (ls, rs) = selecter gs in
          if null rs then Nothing else Just (ls, head rs, tail rs)

        go g' env' ls rs g ancs isFirstTime =
          -- trace (printf "\nGo:\ng': %s\nls: %s\nrs: %s\n" (show g') (show ls) (show rs)) $
          let normalized = normalize g' in
          -- trace (printf "normalized: %s" $ show normalized) $
          let unified = mapMaybe (unifyStuff s) normalized in
          -- trace (printf "unified: %s" $ intercalate "\n" $ map show unified) $
          let addDescends xs s =
                substituteDescend s (ls ++ map (\x -> Descend x (Set.insert g ancs)) xs ++ rs) in
          case unified of
            [] ->
              -- trace "fail" $
              Fail
            ns | length ns == 1 || isFirstTime -> -- unfold only if it's deterministic or hasn't been unfolded before
              trace (printf "rs:%s\nns:\n%s\nisFirstTime:\n%s" (intercalate " \n" $ map (show . getCurr) rs) (intercalate " \n" $ map (show . fst) ns) (show isFirstTime)) $
              Or (map step ns) s
              where
                step (xs, s') =
                  if null xs && null rs
                  then Success s'
                  else let newDescends = addDescends xs s' in
                       -- trace (printf "\n\nConj\nNew descends: %s" (show newDescends)) $
                       Conj (sldResolutionStep newDescends env' s' (Set.insert (map getCurr gs) seen) (isFirstTime && length ns == 1)) newDescends s'
            ns | not $ null rs ->
              trace (printf "\nnot null\nns: %s\nls: %s\nrs: %s\ng: %s" (show ns) (show ls) (show rs) (show g)) $
              maybe (Leaf gs s env)
                    (\(ls', Descend nextAtom nextAtomsAncs, rs')  ->
                            let (g'', env'') = unfold nextAtom env in
                            -- trace (printf "\nls' %s\nls: %s" (show ls') (show (ls ++ (Descend g ancs : ls')))) $
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
normalize _ = error "Unexpected goal type in normalization"

unifyStuff :: E.Sigma -> [G S] -> Maybe ([G S], E.Sigma)
unifyStuff state gs = go gs state [] where
  go [] state conjs = Just (reverse conjs, state)
  go (g@(Invoke _ _) : gs) state conjs = go gs state (g : conjs)
  go ((t :=: u) : gs) state conjs = do
    s <- E.unify  (Just state) t u
    go gs s conjs

bodies :: SldTree -> [[G S]]
bodies = leaves

leaves :: SldTree -> [[G S]]
leaves (Or disjs _)   = concatMap leaves disjs
leaves (Conj ch  _ _) = leaves ch
leaves (Leaf ds _ _)  = [map getCurr ds]
leaves _ = []

resultants :: SldTree -> [(E.Sigma, [G S], Maybe E.Gamma)]
resultants (Success s)     = [(s, [], Nothing)]
resultants (Or disjs _)    = concatMap resultants disjs
resultants (Conj ch _ _)   = resultants ch
resultants (Leaf ds s env) = [(s, map getCurr ds, Just env)]
resultants Fail            = []

topLevel :: G X -> SldTree
topLevel goal =
  let (goal', _, defs) = justTakeOutLets (goal, []) in
  let gamma = E.updateDefsInGamma E.env0 defs in
  let (logicGoal, gamma', names) = E.preEval' gamma goal' in
  sldResolutionStep [Descend logicGoal Set.empty] gamma' E.s0 Set.empty True

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

-- ordered subconjunctions of the proper length
subconjs :: [a] -> Int -> [[a]]
subconjs gs n = filter (\x -> n == length x) $ subsequences gs

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
minimallyGeneral :: (Show a, Ord a) => [[G a]] -> [G a]
minimallyGeneral xs =
  -- trace (printf "minimallyGeneral %s" $ show xs) $
  go xs xs
  where
    go [x] _ = x
    go (x:xs) ys | any (\g -> isStrictInst x g) ys = go xs ys
    go (x:xs) _ = x
    go [] _ = error "Empty list of best matching conjunctions"

bmc :: E.Delta -> [G S] -> [[G S]] -> ([[G S]], E.Delta)
bmc d q [] = ([], d)
bmc d q (q':qCurly) | msgExists q q' =
  -- trace "bmc" $
  let (generalized, _, gen, delta) = D.generalizeGoals d q q' in
  trace (printf "Generalizing\nq:   %s\nq':  %s\nRes: %s\nGen: %s\ndelta: %s\n" (show q) (show q') (show generalized) (show gen) (show $ head d)) $
  let (gss, delta') = bmc delta q qCurly in
  (generalized : gss, delta')
bmc d q (q':qCurly) = trace "why msg does not exist?!" $ bmc d q qCurly
-- bmc d q qCurly = [(\(x,_,_,_) -> x) $ D.generalizeGoals d q q' | q' <- qCurly, msgExists q q']

split :: E.Delta -> [G S] -> [G S] -> (([G S], [G S]), E.Delta)
split d q q' = -- q <= q'
  -- trace (printf "splitting\nq:  %s\nq': %s\n" (show q) (show q')) $
  let n = length q in
  -- let qCurly = filterTrace (\q'' -> q `embed` q'') $ subconjs q' n in
  -- trace (intercalate "\n" $ map show $ map (\q'' -> (q, q'', zipWith embed q q'')) $ subconjs q' n) $
  let qCurly = filter (\q'' -> and $ zipWith embed q q'') $ subconjs q' n in
  -- trace (printf "\nQcurly: %s" (show qCurly)) $
  let (bestMC, delta) = bmc d q qCurly in
  -- trace (printf "\nBMC: %s" $ show bestMC ) $
  let b = minimallyGeneral bestMC in
  -- trace (printf "\nQcurly: %s\nBestMC: %s\nB:  %s\nQ': %s\nQ:  %s\n" (show qCurly) (show bestMC) (show b) (show q') (show q)) $
  ((b, if length q' > n then complementSubconjs b q' else []), delta)

class AlwaysEmbeddable a => Homeo a where
  couple :: a -> a -> Bool
  diving :: a -> a -> Bool
  homeo  :: a -> a -> Bool
  homeo x y = couple x y || diving x y

instance Homeo (Term a) where
  couple (C n as) (C m bs) | n == m && length as == length bs =
    all (uncurry homeo) $ zip as bs
  couple _ _ = False

  diving v (C _ as) = any (homeo v) as
  diving _ _ = False

  homeo (V _) (V _) = True
  homeo x y = couple x y || diving x y

instance Show a => Homeo (G a) where
  couple goal@(Invoke f as) (Invoke g bs) | isAlwaysEmbeddable goal || f == g && length as == length bs =
    all (uncurry homeo) $ zip as bs
  couple _ _ = False

  diving _ _ = False

instance Show a => Homeo [G a] where
  couple = undefined
  diving = undefined

  homeo gs hs =
    any (all (uncurry homeo) . zip gs) (subconjs hs (length gs))
  -- couple gs hs | length gs == length hs =
  --   all (uncurry homeo) $ zip gs hs
  -- couple _ _ = False
  --
  -- diving gs (h:hs) = homeo gs hs
  -- diving _ _ = False

class (Eq b) => Instance a b | b -> a where
  inst :: b -> b -> Map a (Term a) -> Maybe (Map a (Term a))

  isInst :: b -> b -> Bool
  isInst x y = isJust $ inst x y Map.empty

  isStrictInst :: b -> b -> Bool
  isStrictInst x y = isInst x y && not (isInst y x)

  isVariant :: b -> b -> Bool
  isVariant x y = x == y || isInst x y && isInst y x

  isRenaming :: b -> b -> Bool
  isRenaming x y =
    x == y || maybe False (all (\e -> case e of V _ -> True; _ -> False ) . Map.elems) (inst x y Map.empty)

  instanceCheck :: b -> [b] -> Bool
  instanceCheck g = any (isInst g)

  variantCheck :: Foldable t => b -> t b -> Bool
  variantCheck g = any (isVariant g)

instance (Eq a, Ord a) => Instance a (Term a) where
  inst (V v) u subst =
    case Map.lookup v subst of
      Just w | u == w -> Just subst
      Just w -> Nothing
      Nothing -> Just $ Map.insert v u subst
  inst (C n as) (C m bs) subst | length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing

instance (Eq a, Ord a) => Instance a (G a) where
  inst (Invoke f as) (Invoke g bs) subst | f == g && length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing


instance (Eq a, Ord a) => Instance a [G a] where
  inst as bs subst | length as == length bs =
    foldl (\s (a, b) -> s >>= \s -> inst a b s) (Just subst) (zip as bs)
  inst _ _ _ = Nothing

class AlwaysEmbeddable a where
  isAlwaysEmbeddable :: a -> Bool

instance AlwaysEmbeddable (G a) where
  isAlwaysEmbeddable (Invoke f _) = f `elem` [] --[] -- ["leo", "gto"]
  isAlwaysEmbeddable _ = False

instance AlwaysEmbeddable [G a] where
  isAlwaysEmbeddable = null

instance AlwaysEmbeddable (Term a) where
  isAlwaysEmbeddable = const True

-- Strict homeomorphic embedding. Explore: use a variants check instead of the instance check.
class (Homeo b, Instance a b, Eq b, Show a) => Embed a b | b -> a where
  embed :: b -> b -> Bool
  embed g h = isAlwaysEmbeddable g || g == h || homeo g h && not (isStrictInst h g)

instance (Ord a, Eq a, Show a) => Embed a (G a)
instance (Ord a, Eq a, Show a) => Embed a [G a] where
  embed gs hs =
    let subs = subconjs hs (length gs) in
    any (and . zipWith embed gs) subs
