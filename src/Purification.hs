module Purification where

import Syntax
import Tree
import Driving
import Data.List
import Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import Debug.Trace
import List hiding (c)
import Text.Printf
import Miscellaneous

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
purification x = trace_pur x $
  --identity x
  --justTakeOutLets x
  --purification_old x
  --purificationWithErasure x
  conservativePurificationWithErasure x

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}
identity :: (G X, [String]) -> (G X, [String], [Def])
identity (g, a) = (g, a, [])

{-------------------------------------------}
justTakeOutLets :: (G X, [String]) -> (G X, [String], [Def])
justTakeOutLets (goal, args) = (goalWithoutLets, args, defs) where
  initialFvs                 = [0..]
  (goalWithoutClashs,  fvs ) = renameLetArgs initialFvs goal
  (goalWithClosedLets, fvs') = escapeFreeVars fvs goalWithoutClashs
  (goalWithoutLets,    defs) = takeOutLets goalWithClosedLets

{-------------------------------------------}
purification_old :: (G X, [String]) -> (G X, [String], [Def])
purification_old (goal, args) =
  let purG = totalPurification goalWithClosedLets args in
  let (res, defs) = takeOutLets purG in
  (res, args, defs) where

  {-------------------------------------------}
  getEssentialVars :: G X -> Set X -> Set X
  getEssentialVars (g1 :/\: g2)    s = getEssentialVars g2 $ getEssentialVars g1 s
  getEssentialVars (g1 :\/: g2)    s = getEssentialVars g2 $ getEssentialVars g1 s
  getEssentialVars (Let _ g)       s = getEssentialVars g s
  getEssentialVars (t1 :=:  t2)    s = let vars = Set.fromList $ fv t1 ++ fv t2 in
                                       if Set.null $ Set.intersection s vars then s else Set.union vars s
  getEssentialVars (Invoke _ args) s = let vars = Set.fromList $ concatMap fv args in Set.union vars s
  getEssentialVars (Fresh _ g)     s = getEssentialVars g s

  {-------------------------------------------}
  getAllEssentialVars :: G X -> Set X -> Set X
  getAllEssentialVars g s = let s' = getEssentialVars g s in if s == s' then s else getAllEssentialVars g s'

  {-------------------------------------------}
  findQuasiEssentialVars :: G X -> (Set X, Set X)
  findQuasiEssentialVars (t1 :=:  t2) = (Set.fromList $ fv t1, Set.fromList $ fv t2)
  findQuasiEssentialVars (Invoke _ a) = (Set.empty, Set.fromList $ concatMap fv a)
  findQuasiEssentialVars (g1 :\/: g2) = let (qevs1, evs1) = findQuasiEssentialVars g1 in
                                        let (qevs2, evs2) = findQuasiEssentialVars g2 in
                                        let evs           = Set.union evs1 evs2 in
                                        (Set.union qevs1 qevs2 Set.\\ evs, evs)
  findQuasiEssentialVars (g1 :/\: g2) = let (qevs1, evs1) = findQuasiEssentialVars g1 in
                                        let (qevs2, evs2) = findQuasiEssentialVars g2 in
                                        let evs           = Set.union (Set.union evs1 evs2) $ Set.intersection qevs1 qevs2 in
                                        (Set.union qevs1 qevs2 Set.\\ evs, evs)
  findQuasiEssentialVars (Fresh _ g)  = findQuasiEssentialVars g
  findQuasiEssentialVars (Let _ g)    = findQuasiEssentialVars g

  {-------------------------------------------}
  removeUnessentialVars :: Set X -> G X -> G X
  removeUnessentialVars args g =
    let essentialVars      = getAllEssentialVars g args in
    let allVars            = Set.fromList $ fvg g in
    let unessentialVars    = allVars Set.\\ essentialVars in
    let p1 t1 t2           = Set.null $ Set.intersection essentialVars $ Set.fromList $ fv t1 ++ fv t2 in
    let g0                 = removeUnifications p1 g in
    let quasiEssentialVars = fst (findQuasiEssentialVars g0) Set.\\ args in
    let p2 t1 t2           = not $ Set.null $ Set.intersection quasiEssentialVars $ Set.fromList $ fv t1 ++ fv t2 in
    let g1                 = removeUnifications p2 g0 in --trace ("step: " ++ show unessentialVars ++ ", " ++ show quasiEssentialVars) $
    if Set.size unessentialVars == 0 && Set.size quasiEssentialVars == 0 then g1 else removeUnessentialVars args g1

  {-------------------------------------------}
  purifyInternalLets :: G X -> G X
  purifyInternalLets (g1 :\/: g2)        = purifyInternalLets g1 ||| purifyInternalLets g2
  purifyInternalLets (g1 :/\: g2)        = purifyInternalLets g1 &&& purifyInternalLets g2
  purifyInternalLets (Fresh n g)         = Fresh n $ purifyInternalLets g
  purifyInternalLets (Let (n, a, g1) g2) = Let (n, a, totalPurification g1 a) $ purifyInternalLets g2
  purifyInternalLets g                   = g

  {-------------------------------------------}
  totalPurification :: G X -> [X] -> G X
  totalPurification g a =
    let args      = Set.fromList a in
    let g0        = snd $ freshVars [] g in
    let purifiedG = removeUnessentialVars args g0 in
    let inlinedG  = removeLinks args purifiedG in
    let p t1 t2   = case (t1, t2) of { (V x1, V x2) -> x1 == x2; _ -> False } in
    let filteredG = removeUnifications p inlinedG in
    let totalG    = fresh (fvg filteredG \\ a) filteredG in
    purifyInternalLets totalG

  {-------------------------------------------}

  initialFvs                 = [0..]
  (goalWithoutClashs,  fvs ) = renameLetArgs initialFvs goal
  (goalWithClosedLets, fvs') = escapeFreeVars fvs goalWithoutClashs

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

purificationWithErasure :: (G X, [String]) -> (G X, [String], [Def])
purificationWithErasure x = (g''', args', defs''') where

  (goalWithoutLets, args, defs) = justTakeOutLets x

  mainFuncs      = defToRules ("main", args, goalWithoutLets)
  internalFuncs  = map defToRules defs
  initialErasure = Map.fromList $ map (\(n,a,_) -> (n, [1..length a])) defs
  erasure        = removeRedundantArgs (mainFuncs ++ concat internalFuncs) initialErasure

  mainFuncs'     = map (applyErasureToRule erasure) mainFuncs
  internalFuncs' = map (map (applyErasureToRule erasure)) internalFuncs

  defs'          = map rulesToDef internalFuncs'
  (_,args',g')   = rulesToDef mainFuncs'

  defs''         = map (\(n, a, g) -> (n, a, removeSuccess $ removeLinks (Set.fromList a) g)) defs'
  g''            = removeSuccess $ removeLinks (Set.fromList args') g'

  defs'''        = map (\(n, a, g) -> (n, a, renameFreshVars $ closeByFresh a g)) defs''
  g'''           = renameFreshVars $ closeByFresh args' g''

  {-------------------------------------------}
  ruleToOcanren :: [X] -> Rule -> G X
  ruleToOcanren v ((_, a), bd) =
    let unifies = map (\(v, t) -> V v === t) $ zip v a in
    let calls   = map (\(n, a) -> Invoke n a) bd in
    foldl1 (&&&) $ unifies ++ calls

  {-------------------------------------------}
  rulesToDef :: Rules -> Def
  rulesToDef rules@(((n,a),_):_) =
    let v = map (toV "z") [1..length a] in
    let g = foldl1 (|||) $ map (ruleToOcanren v) rules in
    (n, v, g)

  {-------------------------------------------}
  removeSuccess :: G X -> G X
  removeSuccess = removeUnifications (\_ _ -> False)

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}
conservativePurificationWithErasure :: (G X, [String]) -> (G X, [String], [Def])
conservativePurificationWithErasure x = (goalAfterPurification, args, defsAfterPurification) where

  (goalWithoutLets, args, defs) = justTakeOutLets x

  mainFuncs      = defToRules ("main", args, goalWithoutLets)
  internalFuncs  = map defToRules defs
  initialErasure = Map.fromList $ map (\(n,a,_) -> (n, [1..length a])) defs
  erasure        = removeRedundantArgs (mainFuncs ++ concat internalFuncs) initialErasure

  goalAfterPurification  = snd $ purify "main" args goalWithoutLets
  defsAfterPurification  = filter (not . null . snd3) $ map (\(n, a, g) -> let (a', g') = purify n a g in (n, a', g')) defs

  purify :: Name -> [X] -> G X -> ([X], G X)
  purify n a = let a' = applyErasure erasure n a in
    ((,) a') . renameFreshVars . closeByFresh a' . purifyUni a' . applyErasureToG erasure . snd . freshVars []

  purifyUni :: [X] -> G X -> G X
  purifyUni a g = snd $ purifyU (Set.fromList a) [] g where
    purifyU :: Set X -> [G X] -> G X -> ([G X], G X)
    purifyU constrV conjs g@(l@(V x) :=: r@(V y)) = if Set.member y constrV then
                                                      if Set.member x constrV then (conjs, g)
                                                      else (map (subst_in_goal x r) conjs, success)
                                                    else (map (subst_in_goal y l) conjs, success)
    purifyU constrV conjs g@(V x :=: t)           = if Set.member x constrV then (conjs, g)
                                                    else (map (subst_in_goal x t) conjs, success)
    purifyU constrV conjs (g1 :\/: g2)            = let constrV' = foldl (\s -> Set.union s . Set.fromList . fvg) constrV conjs in
                                                    let ([], g1') = purifyU constrV' [] g1 in
                                                    let ([], g2') = purifyU constrV' [] g2 in
                                                    case (g1', g2') of
                                                      (Invoke "success" [], Invoke "success" []) -> (conjs, success)
                                                      (_                  , Invoke "success" []) -> (conjs, g1')
                                                      (Invoke "success" [], _                  ) -> (conjs, g2')
                                                      _                                          -> (conjs, g1' ||| g2')
                                                    -- (conjs, g1' ||| g2')
    purifyU constrV conjs (g1 :/\: g2)            = let (g2' :conjs' , g1' ) = purifyU constrV (g2 :conjs ) g1  in
                                                    let (g1'':conjs'', g2'') = purifyU constrV (g1':conjs') g2' in
                                                    case (g1'', g2'') of
                                                      (Invoke "success" [], Invoke "success" []) -> (conjs'', success      )
                                                      (Invoke "success" [], _                  ) -> (conjs'', g2''         )
                                                      (_                  , Invoke "success" []) -> (conjs'', g1''         )
                                                      _                                          -> (conjs'', g1'' &&& g2'')
    purifyU constrV conjs g                       = (conjs, g)

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
applyErasureToG e (g1 :/\: g2) = applyErasureToG e g1 &&& applyErasureToG e g2
applyErasureToG e (g1 :\/: g2) = applyErasureToG e g1 ||| applyErasureToG e g2
applyErasureToG e (Fresh n g)  = Fresh n $ applyErasureToG e g
applyErasureToG e g            = g

{-------------------------------------------}
applyErasure :: Erasure -> Name -> [a] -> [a]
applyErasure e n a =
  case Map.lookup n e of
    Nothing -> a
    Just i  -> removeElems i a

{-------------------------------------------}
removeElems :: [Int] -> [a] -> [a]
removeElems rs es = remove 1 rs es where
  remove _   []     es     = es
  remove i l@(r:rs) (e:es) = if i == r then remove (i+1) rs es else e : remove (i+1) l es
  remove _ _      _        = error "Index is out of elements."

{-------------------------------------------}
defToRules :: Def -> Rules
defToRules (n, a, g) = map (\(s, f) -> (applyInFunc s (n, ta), map (applyInFunc s) f)) $ gToRules g where
  ta = map V a

  gToRules :: G X -> [(Subst, Funcs)]
  gToRules ((V v) :=: t) = [([(v, t)], [])]
  gToRules (Invoke n a)  = [([], [(n, a)])]
  gToRules (Fresh _ g)   = gToRules g
  gToRules (g1 :\/: g2)  = gToRules g1 ++ gToRules g2
  gToRules (g1 :/\: g2)  = [(s1 ++ s2, f1 ++ f2) | (s1, f1) <- gToRules g1, (s2, f2) <- gToRules g2]

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
closeByFresh a g = fresh (fvg g \\ a) g

{-------------------------------------------}
success :: G a
success = call "success" []

{-------------------------------------------}
takeOutLets :: G X -> (G X, [Def])
takeOutLets (g1 :/\: g2)        = let (g1', lets1) = takeOutLets g1 in
                                  let (g2', lets2) = takeOutLets g2 in
                                  (g1' &&& g2', lets1 ++ lets2)
takeOutLets (g1 :\/: g2)        = let (g1', lets1) = takeOutLets g1 in
                                  let (g2', lets2) = takeOutLets g2 in
                                  (g1' ||| g2', lets1 ++ lets2)
takeOutLets (Fresh n g)         = let (g', lets) = takeOutLets g in
                                  (Fresh n g', lets)
takeOutLets (Let (n, a, g1) g2) = let (g1', lets1) = takeOutLets g1 in
                                  let (g2', lets2) = takeOutLets g2 in
                                  (g2', (n, a, g1') : lets1 ++ lets2)
takeOutLets g                   = (g, [])

{-------------------------------------------}
renameLetArgs :: [Id] -> G X -> (G X, [Id])
renameLetArgs fvs (g1 :/\: g2)        = let (g1', fvs')   = renameLetArgs fvs  g1 in
                                        let (g2', fvs'')  = renameLetArgs fvs' g2 in
                                        (g1' &&& g2', fvs'')
renameLetArgs fvs (g1 :\/: g2)        = let (g1', fvs')   = renameLetArgs fvs  g1 in
                                        let (g2', fvs'')  = renameLetArgs fvs' g2 in
                                        (g1' ||| g2', fvs'')
renameLetArgs fvs (Fresh n g)         = let (g', fvs')    = renameLetArgs fvs g in
                                        (Fresh n g', fvs')
renameLetArgs fvs (Let (n, a, g1) g2) = let (na, fvs')    = splitAt (length a) fvs in
                                        let na'           = map (toV "y") na in
                                        let ng1           = foldr (\(o, n) -> subst_in_goal o $ V n) g1 $ zip a na' in
                                        let (ng1', fvs'') = renameLetArgs fvs' ng1 in
                                        let (g2', fvs''') = renameLetArgs fvs'' g2 in
                                        (Let (n, na', ng1') g2', fvs''')
renameLetArgs fvs x                   = (x, fvs)


{-------------------------------------------}
escapeFreeVars :: [Id] -> G X -> (G X, [Id])
escapeFreeVars fvs (g1 :/\: g2)        = let (g1', fvs')   = escapeFreeVars fvs  g1 in
                                         let (g2', fvs'')  = escapeFreeVars fvs' g2 in
                                         (g1' &&& g2', fvs'')
escapeFreeVars fvs (g1 :\/: g2)        = let (g1', fvs')   = escapeFreeVars fvs  g1 in
                                         let (g2', fvs'')  = escapeFreeVars fvs' g2 in
                                         (g1' ||| g2', fvs'')
escapeFreeVars fvs (Fresh n g)         = let (g', fvs')    = escapeFreeVars fvs g in
                                         (Fresh n g', fvs')
escapeFreeVars fvs (Let (n, a, g1) g2) = let (g1', fvs')   = escapeFreeVars fvs  g1 in
                                         let (g2', fvs'')  = escapeFreeVars fvs' g2 in
                                         let freeVars      = fvg g1' \\ a in
                                         let (nvs, fvs''') = splitAt (length freeVars) fvs'' in
                                         let nvs'          = map (toV "y") nvs in
                                         let g1''          = addArgsInCall n (map V nvs') g1' in
                                         let g1'''         = foldr (\(o,n) -> subst_in_goal o $ V n) g1'' $ zip freeVars nvs' in
                                         let g2''          = addArgsInCall n (map V freeVars) g2' in
                                         (Let (n, nvs' ++ a, g1''') g2'', fvs''')
escapeFreeVars fvs x                   = (x, fvs)

{-------------------------------------------}
addArgsInCall :: Name -> [Term X] -> G X -> G X
addArgsInCall n na   (g1 :/\: g2)         = addArgsInCall n na g1 &&& addArgsInCall n na g2
addArgsInCall n na   (g1 :\/: g2)         = addArgsInCall n na g1 ||| addArgsInCall n na g2
addArgsInCall n na   (Fresh v g)          = Fresh v $ addArgsInCall n na g
addArgsInCall n na   (Let (n', a, g1) g2) = Let (n', a, addArgsInCall n na g1) $ addArgsInCall n na g2
addArgsInCall n na g@(Invoke n' a)        = if n == n' then Invoke n (na ++ a) else g
addArgsInCall _ _  g                      = g

{-------------------------------------------}
toV :: Show a => String -> a -> String
toV pref = (pref++) . show

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}
getFstLink :: Set X -> (G X -> G X) -> (G X -> G X) -> (G X -> G X) -> G X -> Maybe (G X -> G X, G X, X, Term X)
getFstLink a disjC conjC branch g@(t1@(V x1) :=: t2@(V x2)) =
  if x1 == x2 then Nothing else
    case (Set.member x1 a, Set.member x2 a) of
      (_,     False) -> if hasVar (branch success) x2 then Nothing else Just (disjC, conjC success, x2, t1)
      (False, True)  -> if hasVar (branch success) x1 then Nothing else Just (disjC, conjC success, x1, t2)
      _              -> Nothing
getFstLink a disjC conjC branch g@((V x) :=: t@(C _ _)) =
  if Set.member x a || hasVar (branch success) x || termHasVars t then Nothing else Just (disjC, conjC success, x, t)
getFstLink a disjC conjC branch (g1 :\/: g2) =
  case getFstLink a (disjC . conjC . (||| g2)) id (branch . conjC . (||| success)) g1 of
    v@(Just x) -> v
    Nothing    -> getFstLink a (disjC . conjC . (g1 |||)) id (branch . conjC . (success |||)) g2
getFstLink a disjC conjC branch (g1 :/\: g2) =
  case getFstLink a disjC (conjC . (&&& g2)) branch g1 of
    v@(Just x) -> v
    Nothing    -> getFstLink a disjC (conjC . (g1 &&&)) branch g2
getFstLink a disjC conjC branch (Fresh n g) = getFstLink a disjC (conjC . Fresh n) branch g
getFstLink a disjC conjC branch (Let d g)   = getFstLink a disjC (conjC . Let d) branch g
getFstLink _ _     _     _      _           = Nothing

{-------------------------------------------}
termHasVars :: Term X -> Bool
termHasVars (V _)   = True
termHasVars (C _ a) = any termHasVars a

{-------------------------------------------}
removeLinks :: Set X -> G X -> G X
removeLinks a g = case getFstLink a id id id g of
                    Nothing                  -> g
                    Just (disjC, conj, x, y) -> removeLinks a $ disjC $ subst_in_goal x y conj

{-------------------------------------------}
hasVar g x = elem x $ fvg g

{-------------------------------------------}
removeUnifications :: (Term X -> Term X -> Bool) -> G X -> G X
removeUnifications p g@(t1 :=:  t2) = if p t1 t2 then success else g
removeUnifications p   (g1 :\/: g2) = removeUnifications p g1 :\/: removeUnifications p g2
removeUnifications p   (g1 :/\: g2) = case (removeUnifications p g1, removeUnifications p g2) of
                                        (Invoke "success" [], Invoke "success" []) -> success
                                        (g,                   Invoke "success" []) -> g
                                        (Invoke "success" [], g                  ) -> g
                                        (g1,                  g2                 ) -> g1 :/\: g2
removeUnifications p   (Fresh v g)  = Fresh v $ removeUnifications p g
removeUnifications p   (Let def g)  = Let def $ removeUnifications p g
removeUnifications _ g              = g


renameFreshVars g =
  let (vars, g') = freshVars [] g in
  let vars' = reverse $ map (toV "q") [1..length vars] in
  let g'' = foldr (\(v, v') -> subst_in_goal v (V v')) g' $ zip vars vars' in
  fresh vars'   g''

{-------------------------------------------}
{-------------------------------------------}
{-------------------------------------------}

trace_pur :: (G X, [String]) -> (G X, [String], [Def]) -> (G X, [String], [Def])
trace_pur (g1, _) result@(g2, _, defs) =
  let test_g       = foldr Let g2 defs in
  -- trace ("pur(fresh,unify,arg,constr,var,calls): " ++
  --          show (calc g1) ++
  --          " -> " ++
  --          show (calc test_g)) $
  result where

      (a1,b1,c1,d1,e1,f1) <+> (a2,b2,c2,d2,e2,f2) = (a1+a2,b1+b2,c1+c2,d1+d2,e1+e2,f1+f2)

      constrs (C _ a) = 1 + sum (map constrs a)
      constrs _       = 0

      vars (C _ a) = sum (map constrs a)
      vars _       = 1

      calc (g1 :/\: g2)          = calc g1 <+> calc g2
      calc (g1 :\/: g2)          = calc g1 <+> calc g2
      calc (Fresh _ g)           = (1, 0, 0, 0, 0, 0) <+> calc g
      calc (Let (_, a, g1) g2)   = (0, 0, length a, 0, 0, 0) <+> calc g1 <+> calc g2
      calc (t1 :=: t2)           = (0, 1, 0, constrs t1 + constrs t2, vars t1 + vars t2, 0)
      calc (Invoke "success" []) = (0, 0, 0, 0, 0, 0)
      calc (Invoke _ a)          = (0, 0, 0, sum $ map constrs a, sum $ map vars a, 1)
