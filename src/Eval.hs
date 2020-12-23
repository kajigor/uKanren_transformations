{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Eval where

import           Control.Monad
import           Data.List
import           Stream
import           Syntax
import qualified Data.Map.Strict as Map
import qualified Subst as Subst
import qualified VarInterpretation as VI
import qualified Definitions as Defs
import qualified FreshNames as FN

-- States
-- type Iota  = Map.Map X Ts
-- type Sigma = [(S, Ts)]
-- type Delta = [S]
-- type P     = Map.Map Name Def
type Gamma = (Defs.Definitions, VI.Interpretation, FN.FreshNames)

unifyG :: (S -> Ts -> Subst.Subst -> Bool)
          -> Maybe Subst.Subst -> Ts -> Ts -> Maybe Subst.Subst
unifyG _ Nothing _ _ = Nothing
unifyG f st@(Just subst) u v =
  -- trace (printf "Subst length: %d" (Subst.size subst)) $
  unify' (walk u subst) (walk v subst)  where
    unify' (V u') (V v') | u' == v' = Just subst
    unify' (V u') (V v') = Just $ Subst.insert (min u' v') (V $ max u' v') subst
    unify' (V u') t =
      if f u' t subst
      then Nothing
      else return $ Subst.insert u' v subst
    unify' t (V v') =
      if f v' t subst
      then Nothing
      else return $ Subst.insert v' u subst
    unify' (C a as) (C b bs) | a == b && length as == length bs =
      foldl (\ st' (u', v') -> unifyG f st' u' v') st $ zip as bs
    unify' _ _ = Nothing

walk :: Ts -> Subst.Subst -> Ts
walk x@(V v') s =
  case Subst.lookup v' s of
    Nothing -> x
    Just t  -> walk t s
walk u' _ = u'

-- Unification
unify :: Maybe Subst.Subst -> Ts -> Ts -> Maybe Subst.Subst
unify =
    unifyG occursCheck
  where
    occursCheck :: S -> Ts -> Subst.Subst -> Bool
    occursCheck u' t s =
      let t' = walk t s in
      case t' of
        V v' | v' == u' -> True
        V _             -> False
        C _ as          -> any (\x -> occursCheck u' x s) as

    -- occursCheck u' t s = if elem u' $ fv t then Nothing else s

unifySubsts :: Subst.Subst -> Subst.Subst -> Maybe Subst.Subst
unifySubsts (Subst.Subst one) (Subst.Subst two) =
    -- trace ("one: " ++ show one ++ "\ntwo: " ++ show two) $
    let maximumVar = max (findUpper one) (findUpper two) in
    let one' = manifactureTerm maximumVar one in
    let two' = manifactureTerm maximumVar two in
    unify (Just Subst.empty) one' two'
  where
    findUpper subst =
      if Map.null subst
      then 0
      else fst $ Map.findMax subst
    supplement upper lst = lst --  [(x, y) | x <- [0..upper], let y = maybe (V x) id (lookup x lst)]
    manifactureTerm upper subst = C "ManifacturedTerm" $ Map.elems $ supplement upper subst

unifyNoOccursCheck :: Maybe Subst.Subst -> Ts -> Ts -> Maybe Subst.Subst
unifyNoOccursCheck = unifyG (\_ _ -> const False)


-- Pre-evaluation
preEval :: Gamma -> G X -> (G S, Gamma, [S])
preEval =
    go []
  where
    go vars g@(_, i, _) (t1 :=: t2) =
      (i VI.<@> t1 :=: i VI.<@> t2, g, vars)
    go vars g (g1 :/\: g2) =
      let (g1', g' , vars' ) = go vars  g  g1 in
      let (g2', g'', vars'') = go vars' g' g2 in
      (g1' :/\: g2', g'', vars'')
    go vars g (g1 :\/: g2) =
      let (g1', g' , vars')  = go vars  g  g1 in
      let (g2', g'', vars'') = go vars' g' g2 in
      (g1' :\/: g2', g'', vars'')
    go vars (p, i , d) (Fresh x g') =
      let (y, d') = FN.getFreshName d in
      go (y : vars) (p, VI.extend i x (V y), d') g'
    go vars g@(_, i, _) (Invoke f fs)  =
      (Invoke f (map (i VI.<@>) fs), g, vars)


postEval :: [X] -> G X -> G X
postEval as goal =
  let freshs = fvg goal \\ as in
  foldr Fresh (go (freshs ++ as) goal) freshs
  where
    go vars (g1 :/\: g2) = go vars g1 :/\: go vars g2
    go vars (g1 :\/: g2) = go vars g1 :\/: go vars g2
    go _ g               = g

closeFresh :: [X] -> G X -> G X
closeFresh as goal = goal
  --   let goalNoFresh = stripFresh goal in
  --   trace (printf "\n========================================\nCloseFresh\nBefore\n%s\nAfter\n%s\n" (show goal) (show goalNoFresh)) $
  --   -- let (f, xs) = go as goal in
  --   let (f, xs) = go as goalNoFresh in
  --   let res = f [] in
  --   trace (printf "\nResult:\n%s\n" (show res)) $
  --   res
  -- where
  --   go as (g :/\: h) =
  --     let (f1, uv1) = go as g in
  --     let (f2, uv2) = go as h in
  --     let uv = union uv1 uv2 in
  --     let func = (\dv ->
  --                     let udv = uv \\ dv in
  --                     trace (printf "In conj\n%s\nudv: %s\n" (show goal) (show udv)) $
  --                     let goal = (f1 (udv ++ dv) :/\: f2 (udv ++ dv)) in
  --                     surrFresh goal udv) in
  --     (func, uv)
  --   go as (g :\/: h) =
  --     let (f1, uv1) = go as g in
  --     let (f2, uv2) = go as h in
  --     let func = (\dv -> f1 dv :\/: f2 dv) in
  --     (func, [])
  --   go as g = -- @(_ :=: _) = or @(Invoke _ _)
  --     let fresh = getFresh g as in
  --     let func = (\dv -> let fs = fresh \\ dv in surrFresh g fs) in
  --     (func, fresh)

  --   stripFresh (Fresh _ g) = stripFresh g
  --   stripFresh g = g

  --   surrFresh goal [] = goal
  --   surrFresh goal vs = foldr Fresh goal vs

  --   getFresh goal as = fvg goal \\ as



topLevel :: Program -> Stream (Subst.Subst, FN.FreshNames)
topLevel (Program defs goal) =
  let gamma = foldl update env0 defs in
  let (goal', gamma', _) = preEval gamma goal in
  eval gamma' Subst.empty goal'

-- Evaluation relation
eval :: Gamma -> Subst.Subst -> G S -> Stream (Subst.Subst, FN.FreshNames)
eval     (_, _, d) s (t1 :=:  t2)  = fmap (,d) (maybeToStream $ unify (Just s) t1 t2)
eval env           s (g1 :\/: g2)  = eval env s g1 `mplus` eval env s g2
eval env@(p, i, _) s (g1 :/\: g2)  = eval env s g1 >>= (\ (s', d') -> eval (p, i, d') s' g2)
eval     (p, i, d) s (Invoke f as) =
  let (Def _ fs g) = Defs.getDef p f in
  let i'         = foldl (\ i'' (f', a) -> VI.extend i'' f' a) i $ zip fs as in
  let (g', env', _) = preEval (p, i', d) g in
  eval env' s g'
eval _ _ _ = error "Impossible case in eval"

env0 :: Gamma
env0 = (Defs.empty, VI.empty, FN.defaultNames)

update :: Gamma -> Def -> Gamma
update (p, i, d) def@(Def name _ _) = (Defs.insert name def p, i, d)

updateDefsInGamma :: Gamma -> [Def] -> Gamma
updateDefsInGamma = foldl update

gammaFromDefs :: [Def] -> Gamma
gammaFromDefs = updateDefsInGamma env0

run :: Program -> Stream Subst.Subst
run (Program defs goal) =
  let env = gammaFromDefs defs in
  let (goal', env', _) = preEval env goal in
  fmap fst $ eval env' Subst.empty goal'
