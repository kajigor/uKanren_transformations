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

type Set = Set.Set

-- Purification of non-essential variables
purification :: (G X, [String]) -> (G X, [String])
purification (goal, args) =
  let purG = totalPurification goalWithClosedLets args in
  (trace ("purification(fresh,unify): " ++ show (countFVandUni goal) ++ " -> " ++ show (countFVandUni purG)) purG, args) where

  add (a, b) (c, d) = (a + c, b + d)

  countFVandUni (g1 :/\: g2)        = countFVandUni g1 `add` countFVandUni g2
  countFVandUni (g1 :\/: g2)        = countFVandUni g1 `add` countFVandUni g2
  countFVandUni (Fresh _ g)         = (1, 0) `add` countFVandUni g
  countFVandUni (Let (_, _, g1) g2) = countFVandUni g1 `add` countFVandUni g2
  countFVandUni (_ :=: _)           = (0, 1)
  countFVandUni _                   = (0, 0)

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
                                          let na'           = map toV na in
                                          let ng1           = foldr (\(o, n) -> subst_in_goal o $ V n) g1 $ zip a na' in
                                          let (ng1', fvs'') = renameLetArgs fvs' ng1 in
                                          let (g2', fvs''') = renameLetArgs fvs'' g2 in
                                          (Let (n, na', ng1') g2', fvs''')
  renameLetArgs fvs x                   = (x, fvs)

  {-------------------------------------------}
  addArgsInCall :: Name -> [Term X] -> G X -> G X
  addArgsInCall n na   (g1 :/\: g2)         = addArgsInCall n na g1 &&& addArgsInCall n na g2
  addArgsInCall n na   (g1 :\/: g2)         = addArgsInCall n na g1 ||| addArgsInCall n na g2
  addArgsInCall n na   (Fresh v g)          = Fresh v $ addArgsInCall n na g
  addArgsInCall n na   (Let (n', a, g1) g2) = Let (n', a, addArgsInCall n na g1) $ addArgsInCall n na g2
  addArgsInCall n na g@(Invoke n' a)        = if n == n' then Invoke n (na ++ a) else g
  addArgsInCall _ _  g                      = g

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
                                           let nvs'          = map toV nvs in
                                           let g1''          = addArgsInCall n (map V nvs') g1' in
                                           let g1'''         = foldr (\(o,n) -> subst_in_goal o $ V n) g1'' $ zip freeVars nvs' in
                                           let g2''          = addArgsInCall n (map V freeVars) g2' in
                                           (Let (n, nvs' ++ a, g1''') g2'', fvs''')
  escapeFreeVars fvs x                   = (x, fvs)

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
  removeUnifications :: (Term X -> Term X -> Bool) -> G X -> G X
  removeUnifications p g@(t1 :=:  t2) = if p t1 t2 then success' else g
  removeUnifications p   (g1 :\/: g2) = removeUnifications p g1 :\/: removeUnifications p g2
  removeUnifications p   (g1 :/\: g2) = case (removeUnifications p g1, removeUnifications p g2) of
                                          (Invoke "success" [], Invoke "success" []) -> success'
                                          (g,                   Invoke "success" []) -> g
                                          (Invoke "success" [], g                  ) -> g
                                          (g1,                  g2                 ) -> g1 :/\: g2
  removeUnifications p   (Fresh v g)  = Fresh v $ removeUnifications p g
  removeUnifications p   (Let def g)  = Let def $ removeUnifications p g
  removeUnifications _ g              = g

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

  hasVar g x = elem x $ fvg g

  {-------------------------------------------}
  getFstLink :: Set X -> (G X -> G X) -> (G X -> G X) -> (G X -> G X) -> G X -> Maybe (G X -> G X, G X, X, Term X)
  getFstLink a disjC conjC branch g@(t1@(V x1) :=: t2@(V x2)) =
    if x1 == x2 then Nothing else
      case (Set.member x1 a, Set.member x2 a) of
        (_,     False) -> if hasVar (branch success') x2 then Nothing else Just (disjC, conjC success', x2, t1)
        (False, True)  -> if hasVar (branch success') x1 then Nothing else Just (disjC, conjC success', x1, t2)
        _              -> Nothing
  getFstLink a disjC conjC branch g@((V x) :=: t@(C _ _)) =
    if Set.member x a || hasVar (branch success') x || termHasVars t then Nothing else Just (disjC, conjC success', x, t)
  getFstLink a disjC conjC branch (g1 :\/: g2) =
    case getFstLink a (disjC . conjC . (||| g2)) id (branch . conjC . (||| success')) g1 of
      v@(Just x) -> v
      Nothing    -> getFstLink a (disjC . conjC . (g1 |||)) id (branch . conjC . (success' |||)) g2
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
  toV :: Show a => a -> String
  toV = ('y':) . show

  {--------------------------------------------
  unnestLets :: G X -> G X
  unnestLets g = let (g', lets) = unnestLets' g in foldl (flip Let) g' lets where
    unnestLets' :: G X -> (G X, [Def])
    unnestLets' (g1 :/\: g2)        = let (g1', lets1) = unnestLets' g1 in
                                      let (g2', lets2) = unnestLets' g2 in
                                      (g1' &&& g2', lets1 ++ lets2)
    unnestLets' (g1 :\/: g2)        = let (g1', lets1) = unnestLets' g1 in
                                      let (g2', lets2) = unnestLets' g2 in
                                      (g1' ||| g2', lets1 ++ lets2)
    unnestLets' (Fresh n g)         = let (g', lets) = unnestLets' g in
                                      (Fresh n g', lets)
    unnestLets' (Let (n, a, g1) g2) = let (g1', lets1) = unnestLets' g1 in
                                      let (g2', lets2) = unnestLets' g2 in
                                      (g2', (n, a, g1') : lets1 ++ lets2)
    unnestLets' g                   = (g, [])

  --------------------------------------------}

  success'                   = call "success" []
  initialFvs                 = [0..]
  (goalWithoutClashs,  fvs ) = renameLetArgs initialFvs goal
  (goalWithClosedLets, fvs') = escapeFreeVars fvs goalWithoutClashs
