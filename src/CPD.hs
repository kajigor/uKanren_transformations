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
import qualified Driving as D

data Descend = Descend { getGoal :: G S, getAncs :: [G S] } deriving (Show, Eq)

data SldTree = Fail
             | Success E.Sigma
             | Or [SldTree] E.Sigma
             | Conj SldTree [Descend] E.Sigma
             | Leaf [Descend] E.Sigma

select :: [Descend] -> Maybe Descend
select = find (\x -> isSelectable embed (getGoal x) (getAncs x))

selecter :: [Descend] -> ([Descend], [Descend])
selecter ds = span (\x -> not $ isSelectable embed (getGoal x) (getAncs x)) ds
  -- trace (printf "Selecting in %s" (show $ map getGoal ds)) $
  -- let (x, y) = span (\x -> not $ isSelectable embed (getGoal x) (getAncs x)) ds in
  -- trace (printf "Result:\n%s\n%s" (show $ map getGoal x) (show $ map getGoal y)) (x,y)

-- TODO reconsider hardcoded list of basic function names
isSelectable :: (G a -> G a -> Bool) -> G a -> [G a] -> Bool
isSelectable emb goal ancs =
  not $ any (`emb` goal) ancs -- && fineToUnfold goal
  -- where
  --   fineToUnfold (Invoke f _) = f `notElem` basics
  --   fineToUnfold _ = False
  --   basics = ["leo", "gto"]

substituteDescend s = map $ \(Descend g ancs) -> Descend (E.substituteGoal s g) ancs

sldResolutionStep :: [Descend] -> E.Gamma -> E.Sigma -> [[G S]] ->SldTree
sldResolutionStep gs env@(p, i, d) s seen =
  if variantCheck (map getGoal gs) seen
  then Leaf gs s
  else
    case selecter gs of
      (_, []) -> Leaf gs s
      (ls, Descend g@(Invoke f as) ancs : rs) ->
        let (_, fs, body) = p f in
        if length fs == length as
        then
          let i' = foldl (\ interp (f, a) -> E.extend interp f a) i $ zip fs as in
          let (g', env', _) = E.preEval' (p, i', d) body in
          let normalized = normalize g' in
          let unified = mapMaybe (unifyStuff s) normalized in
          let addDescends xs s = substituteDescend s (ls ++ map (\x -> Descend x (g : ancs)) xs ++ rs) in
          case unified of
            [] -> Fail
            ns ->
              Or (map step ns) s
              where
                step ([], s') =
                  case rs of
                    [] -> Success s'
                    _  -> let newDescends = addDescends [] s' in
                          Conj (sldResolutionStep newDescends env' s' (map getGoal gs : seen)) newDescends s'
                step (xs, s') = let newDescends = addDescends xs s'
                                in  Conj (sldResolutionStep newDescends env' s' (map getGoal gs : seen)) newDescends s'
        else error "Unfolding error: different number of factual and actual arguments"

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
    s <- E.unify (Just state) t u
    go gs s conjs

sldResolution :: [Descend] -> SldTree
sldResolution gs = undefined
  --maybe (Leaf gs) (\sel -> undefined) (select gs)

bodies :: SldTree -> [[G S]]
bodies = leaves

leaves :: SldTree -> [[G S]]
leaves (Or disjs _) = concatMap leaves disjs
leaves (Conj ch  _ _) = leaves ch
leaves (Leaf ds _) =  [map getGoal ds]
leaves _ = []

resultants :: SldTree -> [(E.Sigma, [G S])]
resultants (Success s) = [(s, [])]
resultants (Or disjs _) = concatMap resultants disjs
resultants (Conj ch _ _) = resultants ch
resultants (Leaf ds s) = [(s, map getGoal ds)]

unfold :: [G S] -> [[G S]]
unfold gs =
  let trivialDescend = map (\g -> Descend g []) gs in
  leaves $ Conj (sldResolution trivialDescend) trivialDescend E.s0

topLevel :: G X -> SldTree
topLevel goal =
  let (goal', _, defs) = justTakeOutLets (goal, []) in
  let gamma = E.updateDefsInGamma E.env0 defs in
  let (logicGoal, gamma', names) = E.preEval' gamma goal' in
  sldResolutionStep [Descend logicGoal []] gamma' E.s0 []

mcs :: (Eq a, Show a) => [G a] -> [[G a]]
mcs [] = []
mcs [g] = [[g]]
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
  all (\x -> case x of (Invoke f _, Invoke g _) | f == g -> True; _ -> False) $ zip gs hs
msgExists _ _ = False

-- ordered subconjunctions of the proper length
subconjs :: [a] -> Int -> [[a]]
subconjs gs n = filter (\x -> n == length x) $ subsequences gs

-- works for ordered subconjunctions
complementSubconjs :: (Instance a (Term a), Eq a, Ord a, Show a) => [G a] -> [G a] -> [G a]
complementSubconjs [] xs = xs
complementSubconjs (x:xs) (y:ys) | x == y = complementSubconjs xs ys
complementSubconjs (x:xs) (y:ys) | isRenaming x y = complementSubconjs xs ys
complementSubconjs xs (y:ys) = y : complementSubconjs xs ys
-- complementSubconjs xs ys = error (printf "complementing %s by %s" (show xs) (show ys))

-- TODO : implemented literally according to the definition, may be inefficient. Look at the graph approach again.
-- elem q is minimally general of Q iff there doesn't exist another elem q' \in Q which is a strict instance (q' = q \Theta)
-- isStrictInst q t iff q = t \Theta
minimallyGeneral :: Ord a => [[G a]] -> [G a]
minimallyGeneral xs = go xs xs where
  go [x] _ = x
  --minimallyGeneral (x:xs) = if any (\g -> isStrictInst x g) xs then trace (show (map (\g -> (x, g, isStrictInst x g)) xs)) $  minimallyGeneral xs else x
  go (x:xs) ys | any (\g -> isStrictInst x g) ys = go xs ys
  go (x:xs) _ = x
  go [] _ = error "Empty list of best matching conjunctions"

bmc :: E.Delta -> [G S] -> [[G S]] -> [[G S]]
bmc d q qCurly = [(\(x,_,_,_) -> x) $ D.generalizeGoals d q q' | q' <- qCurly, msgExists q q']
  -- let res = [(\(x,_,_,_) -> x) $ D.generalizeGoals d q q' | q' <- qCurly, msgExists q q']
  -- in trace (show res) res

split :: E.Delta -> [G S] -> [G S] -> ([G S], [G S])
split d q q' = -- q <= q'
  let n = length q in
  let qCurly = filter (\q'' -> q `embed` q'') $ subconjs q' n in
  let b = minimallyGeneral $ bmc d q qCurly in
  (b, complementSubconjs b q')

class Homeo a where
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

instance Homeo (G a) where
  couple (Invoke f as) (Invoke g bs) | f == g && length as == length bs =
    all (uncurry homeo) $ zip as bs
  couple _ _ = False

  diving _ _ = False

instance Homeo [G a] where
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

  variantCheck :: b -> [b] -> Bool
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

-- Strict homeomorphic embedding. Explore: use a variants check instead of the instance check.
class (Homeo b, Instance a b, Eq b) => Embed a b | b -> a where
  embed :: b -> b -> Bool
  embed g h = g == h || homeo g h && not (isStrictInst h g)

instance (Ord a, Eq a) => Embed a (G a)
instance (Ord a, Eq a) => Embed a [G a]
