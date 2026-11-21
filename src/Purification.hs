module Purification where

import           Control.Monad.State
import           Control.Monad       (foldM)
import           Data.List
import           Data.Maybe
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Debug.Trace
import           Def
import           Program
import           Syntax
import           Text.Printf         (printf)
import qualified Eval 

type Set = Set.Set
type Map = Map.Map

type Subst       = [(X, Term X)]
type Func        = (Name, [Term X])
type Funcs       = [Func]
type Rule        = (Func, [Func])
type Rules       = [Rule]
type Erasure     = Map Name [Int]
type ErasureElem = (Name, Int)

-- Purification of non-essential variables and arguments
purification :: (Program G X, [String]) -> (G X, [String], [Def G X])
purification (program@(Program defs x), names) =
  -- justTakeOutLetsProgram program names
  -- tracePur (x, names) $
  -- identity x
  --justTakeOutLets x
  --purificationWithErasure x
--  conservativePurificationWithErasure program names
  let (goal', xs, defs') = conservativePurificationWithErasure program names in

  let (goal'', defs'') = until (\(g', df') -> clearPrg (g', df') == (g', df')) clearPrg (goal', defs') in

  (goal'', xs, defs'')

clearPrg :: (G X, [Def G X]) -> (G X, [Def G X])
clearPrg (g, defs) =
  let g' = clearGoal defs g in
  let defs' = mapMaybe (clearDef defs) defs in
  case g' of
    Nothing -> (failure, defs')
    Just g'' -> (g'', defs')

clearDef :: [Def G X] -> Def G X -> Maybe (Def G X)
clearDef defs (Def n a g) = do
   Def n a <$> clearGoal defs g

-- | Remove calls to relations that have no (residual) definition.  Such a
--   call can never succeed, so 'Nothing' stands for failure: it makes the
--   enclosing conjunction fail and is dropped from an enclosing disjunction.
--   (Previously failing conjuncts were silently dropped, which made residual
--   programs produce extra, unsound answers.)
clearGoal :: [Def G X] -> G X -> Maybe (G X)
clearGoal defs g@(Disjunction g1 g2 lstG) =
  case mapMaybe (clearGoal defs) (g1 : g2 : lstG) of
    [] -> Nothing
    [x] -> Just x
    (x1 : x2 : xs) -> Just $ Disjunction x1 x2 xs
clearGoal defs g@(Conjunction g1 g2 lstG) = do
  gs <- mapM (clearGoal defs) (g1 : g2 : lstG)
  return $ unsafeConj gs
clearGoal defs (Delay g) = do
  g' <- clearGoal defs g
  return $ Delay g'
clearGoal defs (Fresh x g) = do
  g' <- clearGoal defs g
  return $ Fresh x g'
clearGoal defs g | isSuccess g = Just g
clearGoal defs g@(Invoke name _) | any (\(Def n a g) -> n == name) defs =
  Just g
clearGoal defs g@(Invoke name _) = Nothing
clearGoal defs g@(t1 :=: t2) = Just g


{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}
identity :: (G X, [String]) -> (G X, [String], [Def G X])
identity (g, a) = (g, a, [])

justTakeOutLetsProgram :: Program G X -> [String] -> (G X, [String], [Def G X])
justTakeOutLetsProgram program args =
    (goalWithoutLets, args, defs)
  where
    initialFvs = [0..]
    (Program defs goalWithoutLets) = evalState state initialFvs
    state = do
      renamed <- renameProgram program
      escapeFreeVarsProgram renamed
      -- evalState (renameProgram program >>= escapeFreeVarsProgram) initialFvs

{-------------------------------------------}
justTakeOutLets :: (G X, [String]) -> (G X, [String], [Def G X])
justTakeOutLets (goal, args) = (goal, args, [])

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

purificationWithErasure :: (G X, [String]) -> (G X, [String], [Def G X])
purificationWithErasure x = (g''', args', defs''') where

  (goalWithoutLets, args, defs) = justTakeOutLets x

  mainFuncs        = defToRules (Def "main" args goalWithoutLets)
  internalFuncs    = map defToRules defs
  initialErasure   = Map.fromList $ map (\(Def n a _) -> (n, [1..length a])) defs
  erasure          = removeRedundantArgs (mainFuncs ++ concat internalFuncs) initialErasure

  mainFuncs'       = map (applyErasureToRule erasure) mainFuncs
  internalFuncs'   = map (map (applyErasureToRule erasure)) internalFuncs

  defs'            = map rulesToDef internalFuncs'
  (Def _ args' g') = rulesToDef mainFuncs'

  defs''           = map (\(Def n a g) -> Def n a (removeSuccess $ removeLinks (Set.fromList a) g)) defs'
  g''              = removeSuccess $ removeLinks (Set.fromList args') g'

  defs'''          = map (\(Def n a g) -> Def n a (renameFreshVars $ closeByFresh a g)) defs''
  g'''             = renameFreshVars $ closeByFresh args' g''

  {-------------------------------------------}
  ruleToOcanren :: [X] -> Rule -> G X
  ruleToOcanren v ((_, a), bd) =
    let unifies = zipWith (\v t -> V v === t) v a in
    let calls   = map (uncurry Invoke) bd in
    unsafeConj $ unifies ++ calls

  {-------------------------------------------}
  rulesToDef :: Rules -> Def G X
  rulesToDef rules@(((n,a),_):_) =
    let v = map (toV "z") [1..length a] in
    let g = unsafeDisj $ map (ruleToOcanren v) rules in
    Def n v g

  {-------------------------------------------}
  removeSuccess :: G X -> G X
  removeSuccess = removeUnifications (\_ _ -> False)

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}
conservativePurificationWithErasure :: Program G X -> [String] -> (G X, [String], [Def G X])
conservativePurificationWithErasure program@(Program defs goal) arguments =
    (goalAfterPurification, args, defsAfterPurification)
  where

    (goalWithoutLets, args, defs) = justTakeOutLetsProgram program arguments

    mainFuncs      = defToRules (Def "main" args goalWithoutLets)
    internalFuncs  = map defToRules defs
    initialErasure = Map.fromList $ map (\(Def n a _) -> (n, [1..length a])) defs
    erasure        = removeRedundantArgs (mainFuncs ++ concat internalFuncs) initialErasure

    goalAfterPurification  = snd $ purify "main" args goalWithoutLets
    defsAfterPurification  =
      let purified = map (\(Def n a g) -> let (a', g') = purify n a g in Def n a' g') defs in
      map (\(Def n a g) -> let (a', g') = purify n a g in Def n a' g') defs

    -- defsAfterPurification  = {- filter (not . null . snd3) $ -} map (\(Def n a g) -> let (a', g') = purify n a g in (Def n a' g')) defs

    purify :: Name -> [X] -> G X -> ([X], G X)
    purify n a g =
      let a' = applyErasure erasure n a in
      (((,) a') . renameFreshVars . closeByFresh a' . purifyUni a' . applyErasureToG erasure . snd . freshVars []) g

    purifyUni :: [X] -> G X -> G X
    purifyUni a g = snd $ purifyU (Set.fromList a) [] g where
      purifyU :: Set X -> [G X] -> G X -> ([G X], G X)
      purifyU constrV conjs g@(l@(V x) :=: r@(V y)) =
        if Set.member y constrV
        then
          if Set.member x constrV
          then (conjs, g)
          else (map (substInGoal x r) conjs, success)
        else (map (substInGoal y l) conjs, success)
      purifyU constrV conjs g@(V x :=: t) =
        if Set.member x constrV
        then (conjs, g)
        else (map (substInGoal x t) conjs, success)
      purifyU constrV conjs (Disjunction x y gs) =
        let constrV' = foldl (\s -> Set.union s . Set.fromList . fv) constrV conjs in
        let gs' = map snd $ purifyU constrV' [] <$> (x : y : gs) in
        let filtered = filter (not . isSuccess) gs' in
        if null gs'
        then (conjs, success)
        else
          if length gs' == 1
          then (conjs, head gs')
          else (conjs, unsafeDisj gs')
      purifyU constrV conjs c@(Conjunction g1 g' gs) =
        let g2 = unsafeConj (g' : gs) in
        let (g2' :conjs' , g1' ) = purifyU constrV (g2 : conjs) g1 in
        let (g1'':conjs'', g2'') = purifyU constrV (g1':conjs') g2' in
        let res = case (g1'', g2'') of
                    _ | isSuccess g1'' && isSuccess g2'' -> (conjs'', success)
                    _ | isSuccess g1'' -> (conjs'', g2'' )
                    _ | isSuccess g2'' -> (conjs'', g1'' )
                    _ -> (conjs'', flatConj g1'' g2'')
        in
        res
      purifyU constrV conjs g = (conjs, g)

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

removeRedundantArgs :: Rules -> Erasure -> Erasure
-- Greatest fixpoint: an element that is bad stays bad when other elements are
-- removed (fewer erased arguments only add occurrences), so every bad element
-- of a round is removed at once (previously one element per full rescan).
removeRedundantArgs r e =
  case filter (isBadErasureElem r e) $ erasureToList e of
    []  -> e
    bad -> removeRedundantArgs r $ foldl' (\m (f, i) -> Map.adjust (delete i) f m) e bad
  where

    {-------------------------------------------}
    erasureToList :: Erasure -> [ErasureElem]
    erasureToList = concatMap (\(n,i) -> map ((,) n) i) . Map.toList

    {-------------------------------------------}
    isBadErasureElem :: Rules -> Erasure -> ErasureElem -> Bool
    isBadErasureElem rules er el = any (isBadForRule er el) rules

    {-------------------------------------------}
    isBadForRule :: Erasure -> ErasureElem -> Rule -> Bool
    isBadForRule er el (hd, bd) = isBadForFunc er el hd [] bd

    {-------------------------------------------}
    isBadForFunc :: Erasure -> ErasureElem -> Func -> Funcs -> Funcs -> Bool
    isBadForFunc _ _ _ _ [] = False
    isBadForFunc er p@(n, i) hd pref (f@(n', a):suff) =
      if n /= n' then isBadForFunc er p hd (f:pref) suff
        else case splitAt (i-1) a of
          (_,  C _ _ : _ ) -> True
          (a1, V v   : a2) ->
            if any (hasVarInTerm v) a1 ||
               any (hasVarInTerm v) a2 ||
               any (hasVarInFunc v) pref ||
               any (hasVarInFunc v) suff ||
               hasVarInFunc v (applyErasureToFunc er hd)
            then True
            else isBadForFunc er p hd (f:pref) suff

    {-------------------------------------------}
    hasVarInFunc :: X -> Func -> Bool
    hasVarInFunc v (_, a) = any (hasVarInTerm v) a

    {-------------------------------------------}
    hasVarInTerm :: X -> Term X -> Bool
    hasVarInTerm v1 (V v2)  = v1 == v2
    hasVarInTerm v  (C _ a) = any (hasVarInTerm v) a

{-------------------------------------------}
applyErasureToRule :: Erasure -> Rule -> Rule
applyErasureToRule e (hd, tl) = (applyErasureToFunc e hd, map (applyErasureToFunc e) tl)

{-------------------------------------------}
applyErasureToFunc :: Erasure -> Func -> Func
applyErasureToFunc e f@(n, args) = (n, applyErasure e n args)

{-------------------------------------------}
applyErasureToG :: Erasure -> G X -> G X
applyErasureToG e (Invoke n a) = Invoke n $ applyErasure e n a
applyErasureToG e (Conjunction x y gs) = unsafeConj $ applyErasureToG e <$> (x : y : gs)
applyErasureToG e (Disjunction x y gs) = unsafeDisj $ applyErasureToG e <$> (x : y : gs)
applyErasureToG e (Fresh n g) = Fresh n $ applyErasureToG e g
applyErasureToG e g = g

{-------------------------------------------}
applyErasure :: Erasure -> Name -> [a] -> [a]
applyErasure e n a =
  case Map.lookup n e of
    Nothing -> a
    Just i  -> removeElems i a

{-------------------------------------------}
removeElems :: [Int] -> [a] -> [a]
removeElems rs es = remove 1 rs es where
  remove _ [] es = es
  remove i l@(r:rs) (e:es) = if i == r then remove (i+1) rs es else e : remove (i+1) l es
  remove _ _ _ = error "Index is out of elements."

{-------------------------------------------}
defToRules :: Def G X -> Rules
-- Every clause (disjunct) becomes a rule whose head and calls are
-- instantiated with the *most general unifier* of the clause's equations.
-- (Previously the equations were used as a list of bindings, so a second
-- binding of the same variable, e.g. x = 4 :: y & x = h :: t, was ignored:
-- h and t then looked unconstrained and arguments carrying them were
-- erased, which made residual programs produce wrong answers.)
-- Clauses whose equations are not unifiable are dropped.
defToRules (Def n a g) =
    [ (applyInFunc s (n, ta), map (applyInFunc s) f)
    | (eqs, f) <- gToRules g
    , Just m <- [mgu eqs]
    , let s = Map.toList m ] where
  ta = map V a

  gToRules :: G X -> [([(Term X, Term X)], Funcs)]
  gToRules (t1 :=: t2) = [([(t1, t2)], [])]
  gToRules (Invoke n a)  = [([], [(n, a)])]
  gToRules (Fresh _ g) = gToRules g
  gToRules (Delay g) = gToRules g
  gToRules (Disjunction x y gs) = concatMap gToRules (x : y : gs)
  gToRules (Conjunction g1 g2 gs) =
    [(s1 ++ s2, f1 ++ f2) | (s1, f1) <- gToRules g1, (s2, f2) <- gToRules (unsafeConj (g2 : gs))]

  mgu :: [(Term X, Term X)] -> Maybe (Map.Map X (Term X))
  mgu = foldM (\m (x, y) -> unify m x y) Map.empty

  walk m t@(V v) = maybe t (walk m) (Map.lookup v m)
  walk _ t = t

  occurs m v t = case walk m t of
    V w -> v == w
    C _ ts -> any (occurs m v) ts

  unify m x y = case (walk m x, walk m y) of
    (V v, V w) | v == w -> Just m
    (V v, t) -> if occurs m v t then Nothing else Just (Map.insert v t m)
    (t, V w) -> if occurs m w t then Nothing else Just (Map.insert w t m)
    (C f as, C h bs) | f == h && length as == length bs -> foldM (\m' (p, q) -> unify m' p q) m (zip as bs)
    _ -> Nothing

  applySubst :: Subst -> Term X -> Term X
  applySubst s t@(V v) = case lookup v s of
                           Nothing -> t
                           Just t' -> applySubst s t'
  applySubst s (C n a) = C n $ map (applySubst s) a

  applyInFunc :: Subst -> Func -> Func
  applyInFunc s (n, a) = (n, map (applySubst s) a)

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

closeByFresh :: [X] -> G X -> G X
-- closeByFresh a g = fresh (fv g \\ a) g
closeByFresh = Eval.bindFresh1

renameGoal :: G X -> Map X X -> G X
renameGoal goal mapping =
    (\x -> fromMaybe x (Map.lookup x mapping)) <$> goal

renameDef :: Def G X -> State [S] (Def G X)
renameDef (Def name args body) = do
    names <- get
    let (newNames, rest) = splitAt (length args) names
    put rest
    let args' = map (toV "y") newNames
    let body' = renameGoal body (Map.fromList (zip args args'))
    return (Def name args' body')

renameDefs :: [Def G X] -> State [S] [Def G X]
renameDefs = mapM renameDef

renameProgram :: Program G X -> State [S] (Program G X)
renameProgram (Program defs goal) = do
    defs' <- renameDefs defs
    return $ Program defs' goal

{-------------------------------------------}
renameLetArgs :: [S] -> G X -> (G X, [S])
renameLetArgs fv x = (x, fv)


escapeFreeVarsDef :: Def G X -> State ([S], Map Name [X]) (Def G X)
escapeFreeVarsDef (Def name args body) = do
    (names, mapping) <- get
    let freeVars = fv body \\ args
    let (newNames, rest) = splitAt (length freeVars) names
    put (rest, Map.insert name freeVars mapping)
    let new = map (toV "y") newNames
    return (Def name (new ++ args) (renameGoal body (Map.fromList $ zip freeVars new)))

escapeFreeVarsProgram :: Program G X -> State [S] (Program G X)
escapeFreeVarsProgram (Program defs goal) = do
    names <- get
    let state = mapM escapeFreeVarsDef defs
    let (defs', (names', mapping)) = runState state (names, Map.empty)
    put names'
    let goal' = addArgsInInvocations mapping goal
    let defs'' = map (\(Def n a b) -> Def n a (addArgsInInvocations mapping b)) defs'
    return (Program defs'' goal')

addArgsInInvocations :: Map Name [X] -> G X -> G X
addArgsInInvocations mapping =
    go
  where
    go (Invoke name args) =
        Invoke name (pref ++ args)
      where
        pref = maybe [] (map V) (Map.lookup name mapping)
    go (Conjunction x y gs) = unsafeConj $ go <$> (x : y : gs)
    go (Disjunction x y gs) = unsafeDisj $ go <$> (x : y : gs)
    go (Fresh n g) = Fresh n $ go g
    go g = g

{-------------------------------------------}
escapeFreeVars :: [S] -> G X -> (G X, [S])
escapeFreeVars fv x = (x, fv)

{-------------------------------------------}
addArgsInCall :: Name -> [Term X] -> G X -> G X
addArgsInCall n na (Conjunction x y gs) = unsafeConj $ addArgsInCall n na <$> (x : y : gs)
addArgsInCall n na (Disjunction x y gs) = unsafeDisj $ addArgsInCall n na <$> (x : y : gs)
addArgsInCall n na (Fresh v g) = Fresh v $ addArgsInCall n na g
addArgsInCall n na g@(Invoke n' a) = if n == n' then Invoke n (na ++ a) else g
addArgsInCall _ _ g = g

{-------------------------------------------}
toV :: Show a => String -> a -> String
toV pref = (pref++) . show

toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}
getFstLink :: Set X -> (G X -> G X) -> (G X -> G X) -> (G X -> G X) -> G X -> Maybe (G X -> G X, G X, X, Term X)
getFstLink a disjC conjC branch g@(t1@(V x1) :=: t2@(V x2)) =
  if x1 == x2
  then Nothing
  else
    case (Set.member x1 a, Set.member x2 a) of
      (_,     False) -> if hasVar (branch success) x2 then Nothing else Just (disjC, conjC success, x2, t1)
      (False, True)  -> if hasVar (branch success) x1 then Nothing else Just (disjC, conjC success, x1, t2)
      _              -> Nothing
getFstLink a disjC conjC branch g@((V x) :=: t@(C _ _)) =
  if Set.member x a || hasVar (branch success) x || termHasVars t
  then Nothing
  else Just (disjC, conjC success, x, t)
getFstLink a disjC conjC branch (Disjunction g1 g2 gs) =
  case getFstLink a (disjC . conjC . (`flatDisj` (unsafeDisj $ g2 : gs))) id (branch . conjC . (`flatDisj` success)) g1 of
    v@(Just x) -> v
    Nothing    -> getFstLink a (disjC . conjC . (g1 `flatDisj`)) id (branch . conjC . (success `flatDisj`)) (unsafeDisj $ g2 : gs)
getFstLink a disjC conjC branch (Conjunction g1 g2 gs) =
  case getFstLink a disjC (conjC . (`flatConj` (unsafeConj $ g2 : gs))) branch g1 of
    v@(Just x) -> v
    Nothing    -> getFstLink a disjC (conjC . (g1 `flatConj`)) branch (unsafeConj $ g2 : gs)
getFstLink a disjC conjC branch (Fresh n g) = getFstLink a disjC (conjC . Fresh n) branch g
getFstLink _ _     _     _      _           = Nothing

{-------------------------------------------}
termHasVars :: Term X -> Bool
termHasVars (V _)   = True
termHasVars (C _ a) = any termHasVars a

{-------------------------------------------}
removeLinks :: Set X -> G X -> G X
removeLinks a g = case getFstLink a id id id g of
                    Nothing                  -> g
                    Just (disjC, conj, x, y) -> removeLinks a $ disjC $ substInGoal x y conj

{-------------------------------------------}
hasVar g x = elem x $ fv g

{-------------------------------------------}
removeUnifications :: (Term X -> Term X -> Bool) -> G X -> G X
removeUnifications p g@(t1 :=: t2) = if p t1 t2 then success else g
removeUnifications p (Disjunction x y gs) = unsafeDisj $ removeUnifications p <$> (x : y : gs)
removeUnifications p (Conjunction x y gs) =
  let gs' = filter (not . isSuccess) $ removeUnifications p <$> (x : y : gs) in
  if null gs'
  then success
  else unsafeConj gs'
removeUnifications p (Fresh v g)  = Fresh v $ removeUnifications p g
removeUnifications _ g = g


renameFreshVars g =
  let (vars, g') = freshVars [] g in
  let vars' = map (toV "q") [1..length vars] in
  let g'' = foldr (\(v, v') -> substInGoal v (V v')) g' $ zip vars vars' in
  fresh vars'   g''

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}
tracePur ::  (Program G X, [String]) -> (G X, [String], [Def G X]) -> (G X, [String], [Def G X])
tracePur (Program defs1 g1, _) result@(g2, _, defs2) =
    trace (printf "pur(fresh,unify,arg,constr,var,calls):\n%s -> %s\n"
            (show $ calcProg $ Program defs1 g1)
            (show $ calcProg $ Program defs2 g2)) $
    result
  where

    (a1,b1,c1,d1,e1,f1) <+> (a2,b2,c2,d2,e2,f2) = (a1+a2,b1+b2,c1+c2,d1+d2,e1+e2,f1+f2)

    constrs (C _ a) = 1 + sum (map constrs a)
    constrs _       = 0

    vars (C _ a) = sum (map constrs a)
    vars _       = 1

    calc (Conjunction x y gs) = foldr1 (<+>) (calc <$> (x : y : gs))
    calc (Disjunction x y gs) = foldr1 (<+>) (calc <$> (x : y : gs))
    calc (Fresh _ g) = (1, 0, 0, 0, 0, 0) <+> calc g
    calc (t1 :=: t2) = (0, 1, 0, constrs t1 + constrs t2, vars t1 + vars t2, 0)
    calc g | isSuccess g = (0, 0, 0, 0, 0, 0)
    calc (Invoke _ a) = (0, 0, 0, sum $ map constrs a, sum $ map vars a, 1)

    calcDef (Def _ a g) = (0, 0, length a, 0, 0, 0) <+> calc g

    calcProg (Program defs goal) =
      calc goal <+> foldr (\d acc -> calcDef d <+> acc) (0,0,0,0,0,0) defs
