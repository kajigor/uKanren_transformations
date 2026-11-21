{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module CPD.LocalControl where

import           Control.Monad.State
import           CPD.State 
import           Data.List           (find, intersect, nub, intercalate)
import           Data.Maybe
import qualified Data.Set            as Set 
import qualified Data.Map            as Map
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
import           Unfold              (getMaximumBranches, normalize, oneStepUnfold, unifyStuff, pruneByLookahead)
import           Util.ListZipper
import qualified Util.Miscellaneous  as Util
import           Debug.Trace

-- | @DeterministicLA k@: deterministic unfolding where resultants that fail
--   within @k@ steps are pruned before counting (see 'Unfold.pruneByLookahead').
--
--   @DeterministicLAP k@: as @DeterministicLA k@, and a conjunction that is an
--   instance of an already processed one only by binding variables to ground
--   terms is specialized on its own instead of being folded into the more
--   general definition (which would discard the static data: @multiply 0 y 4@
--   folded into @multiply x y 4@ keeps a branch that loops without answers).
data Heuristic = Deterministic | Branching | DeterministicLA Int | DeterministicLAP Int deriving (Show, Read)

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

sldResolution :: [G S] -> Subst.Subst S -> Heuristic -> State CPDState SldTree
sldResolution goal subst = sldResolutionStep (map Descend.init goal) subst True

sldResolutionStep :: [DescendGoal] -> Subst.Subst S -> Bool -> Heuristic -> State CPDState SldTree
sldResolutionStep gs s isFirstTime heuristic = do
  env <- gets getEnv
  seen <- gets getSeen
  let currs = map getCurr gs
  if foldable heuristic currs seen
  then return $  Leaf gs s env
  else
      unfoldNext (toZipper gs) isFirstTime gs s
    where
      go g' zipper isFirstTime = do
        env' <- gets getEnv
        let d@(Descend g _) = cursor zipper
        let normalized = normalize g'
        let unified = lookahead heuristic env' (mapMaybe (unifyStuff s) normalized)
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
      needsUnfolding (DeterministicLA _) _ _ ns isFirstTime = length ns == 1 || isFirstTime
      needsUnfolding (DeterministicLAP _) _ _ ns isFirstTime = length ns == 1 || isFirstTime

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
  evalState (sldResolutionStep [Descend.init logicGoal] Subst.empty True heuristic) (CPD.State.init env')

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
minimallyGeneral = minimallyGeneralOn fst

-- | 'minimallyGeneral' on candidates that carry extra data (their positions).
minimallyGeneralOn :: (Show a, Ord a) => (x -> [G a]) -> [x] -> x
minimallyGeneralOn key xs =
  go xs xs
  where
    go [x] _     = x
    go (x:xs) ys | any (\g -> isStrictInst (key x) (key g)) ys = go xs ys
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
  let cands = [ (map fst s, map snd s) | s <- subconjs (zip [0 :: Int ..] q') n
                                        , and (zipWith embed q (map snd s)), msgExists q (map snd s) ] in
  let (bestMC, delta) = bmc d q (map snd cands) in
  let ((b, gen), is) = minimallyGeneralOn (fst . fst) (zip bestMC (map fst cands)) in
  ((b, if length q' > n then [ g | (i, g) <- zip [0 :: Int ..] q', i `notElem` is ] else []), gen, delta)

lookahead :: Heuristic -> Env.Env -> [([G S], Subst.Subst Int)] -> [([G S], Subst.Subst Int)]
lookahead (DeterministicLA k) env = pruneByLookahead k env
lookahead (DeterministicLAP k) env = pruneByLookahead k env
lookahead _ _ = id


-- | Should the conjunction be folded into an already processed one (and not
--   be unfolded)?  'DeterministicLAP' refuses to fold when every binding of the
--   instantiating substitution is a renaming or a ground term and at least one
--   is ground: such a conjunction has strictly fewer variables, and an infinite
--   chain of ground instances along a branch is still stopped by the whistle.
foldable :: Heuristic -> [G S] -> Set.Set [G S] -> Bool
foldable (DeterministicLAP _) currs seen =
  any (\g -> case inst g currs Map.empty of
                Just m  -> not (groundSpecializing (Map.elems m))
                Nothing -> False) seen
  where
    groundSpecializing ts = any isGroundC ts && all (\t -> isVarT t || isGroundC t) ts
    isVarT (V _) = True
    isVarT _     = False
    isGroundC (C _ as) = all isGroundT as
    isGroundC _        = False
    isGroundT (V _)    = False
    isGroundT (C _ as) = all isGroundT as
foldable _ currs seen = instanceCheck currs seen
