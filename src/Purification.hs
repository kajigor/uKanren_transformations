module Purification where

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           Def
import           Program
import           Syntax
import           Text.Printf         (printf)

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
  -- trace_pur (x, names) $
  -- identity x
  --justTakeOutLets x
  --purificationWithErasure x
  conservativePurificationWithErasure program names

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
      escaped <- escapeFreeVarsProgram renamed
      return escaped
      -- evalState (renameProgram program >>= escapeFreeVarsProgram) initialFvs

{-------------------------------------------}
justTakeOutLets :: (G X, [String]) -> (G X, [String], [Def G X])
justTakeOutLets (goal, args) = (goal, args, [])  where

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

  defs''           = map (\(Def n a g) -> (Def n a (removeSuccess $ removeLinks (Set.fromList a) g))) defs'
  g''              = removeSuccess $ removeLinks (Set.fromList args') g'

  defs'''          = map (\(Def n a g) -> (Def n a (renameFreshVars $ closeByFresh a g))) defs''
  g'''             = renameFreshVars $ closeByFresh args' g''

  {-------------------------------------------}
  ruleToOcanren :: [X] -> Rule -> G X
  ruleToOcanren v ((_, a), bd) =
    let unifies = map (\(v, t) -> V v === t) $ zip v a in
    let calls   = map (\(n, a) -> Invoke n a) bd in
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
    defsAfterPurification  = filter (\(Def _ a _) -> not $ null a) $ map (\(Def n a g) -> let (a', g') = purify n a g in (Def n a' g')) defs

    -- defsAfterPurification  = {- filter (not . null . snd3) $ -} map (\(Def n a g) -> let (a', g') = purify n a g in (Def n a' g')) defs

    purify :: Name -> [X] -> G X -> ([X], G X)
    purify n a = let a' = applyErasure erasure n a in
      ((,) a') . renameFreshVars . closeByFresh a' . purifyUni a' . applyErasureToG erasure . snd . freshVars []

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
      -- purifyU constrV conjs (g1 :/\: g2) =
      --   let (g2' :conjs' , g1' ) = purifyU constrV (g2 :conjs ) g1  in
      --   let (g1'':conjs'', g2'') = purifyU constrV (g1':conjs') g2' in
      --   case (g1'', g2'') of
      --     _ | isSuccess g1'' && isSuccess g2'' -> (con{js'', success      )
      --     _ | isSuccess g1'' -> (conjs'', g2'' )
      --     _ | isSuccess g2'' -> (conjs'', g1'' )
      --     _ -> (conjs'', g1'' &&& g2'')
      purifyU constrV conjs g = (conjs, g)

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

removeRedundantArgs :: Rules -> Erasure -> Erasure
removeRedundantArgs r e =
  case find (isBadErasureElem r e) $ erasureToList e of
    Nothing     -> e
    Just (f, i) -> removeRedundantArgs r $ Map.insert f (delete i $ e Map.! f) e
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
defToRules (Def n a g) = map (\(s, f) -> (applyInFunc s (n, ta), map (applyInFunc s) f)) $ gToRules g where
  ta = map V a

  gToRules :: G X -> [(Subst, Funcs)]
  gToRules ((V v) :=: t) = [([(v, t)], [])]
  gToRules (Invoke n a)  = [([], [(n, a)])]
  gToRules (Fresh _ g) = gToRules g
  gToRules (Disjunction x y gs) = concatMap gToRules (x : y : gs)
  gToRules (Conjunction g1 g2 gs) =
    [(s1 ++ s2, f1 ++ f2) | (s1, f1) <- gToRules g1, (s2, f2) <- gToRules (unsafeConj (g2 : gs))]

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
closeByFresh a g = fresh (fv g \\ a) g

renameGoal :: G X -> Map X X -> G X
renameGoal goal mapping =
    (\x -> maybe x id (Map.lookup x mapping)) <$> goal

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
  if (x1 == x2)
  then Nothing
  else
    case (Set.member x1 a, Set.member x2 a) of
      (_,     False) -> if (hasVar (branch success) x2) then Nothing else Just (disjC, conjC success, x2, t1)
      (False, True)  -> if (hasVar (branch success) x1) then Nothing else Just (disjC, conjC success, x1, t2)
      _              -> Nothing
getFstLink a disjC conjC branch g@((V x) :=: t@(C _ _)) =
  if (Set.member x a || hasVar (branch success) x || termHasVars t)
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
trace_pur ::  (Program G X, [String]) -> (G X, [String], [Def G X]) -> (G X, [String], [Def G X])
trace_pur (Program defs1 g1, _) result@(g2, _, defs2) =
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
      (calc goal <+> (foldr (\d acc -> calcDef d <+> acc) (0,0,0,0,0,0) defs))
