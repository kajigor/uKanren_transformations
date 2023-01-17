{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Eval where

import           Control.Monad
import           Data.List
import           Stream
import           Syntax
import           Program
import           Def
import qualified Data.Map.Strict as Map
import qualified Subst
import qualified VarInterpretation as VI
import qualified FreshNames as FN
import qualified Environment as Env
import Control.Monad.State

unifyG :: (Subst.Subst -> S -> Ts -> Bool)
          -> Maybe Subst.Subst -> Ts -> Ts -> Maybe Subst.Subst
unifyG _ Nothing _ _ = Nothing
unifyG f st@(Just subst) u v =
    unify' (walk subst u) (walk subst v)
  where
    unify' (V u') (V v') | u' == v' = Just subst
    unify' (V u') (V v') = Just $ Subst.insert (min u' v') (V $ max u' v') subst
    unify' (V u') t =
      if f subst u' t
      then Nothing
      else return $ Subst.insert u' v subst
    unify' t (V v') =
      if f subst v' t
      then Nothing
      else return $ Subst.insert v' u subst
    unify' (C a as) (C b bs) | a == b && length as == length bs =
      foldl (\ st' (u', v') -> unifyG f st' u' v') st $ zip as bs
    unify' _ _ = Nothing

walk :: Subst.Subst -> Ts -> Ts
walk s x@(V v) =
  maybe x (walk s) $ Subst.lookup v s
walk _ u = u

-- Unification
unify :: Maybe Subst.Subst -> Ts -> Ts -> Maybe Subst.Subst
unify =
    unifyG occursCheck
  where
    occursCheck :: Subst.Subst -> S -> Ts -> Bool
    occursCheck s u t =
      case walk s t of
        V v -> v == u
        C _ as -> any (occursCheck s u) as

    -- occursCheck u' t s = if elem u' $ fv t then Nothing else s

unifySubsts :: Subst.Subst -> Subst.Subst -> Maybe Subst.Subst
unifySubsts (Subst.Subst one) (Subst.Subst two) =
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
preEval :: G X -> State Env.Env (G S, [S])
preEval goal = do
    env <- get
    let (g, (vars, env')) = runState (go goal) ([], env)
    put env'
    return (g, vars)
  where
    go :: G X -> State ([S], Env.Env) (G S)
    go (Fresh x g') = do
      (vars, Env.Env p i d) <- get
      let (y, d') = FN.getFreshName d
      put (y:vars, Env.Env p (VI.extend i x (V y)) d')
      go g'
    go (t1 :=: t2) = do
      i <- getInterp
      return (i VI.<@> t1 :=: i VI.<@> t2)
    go (Invoke f fs) = do
      i <- getInterp
      return (Invoke f (map (i VI.<@>) fs))
    go (Conjunction x y gs) = unsafeConj <$> mapM go (x : y : gs)
    go (Disjunction x y gs) = unsafeDisj <$> mapM go (x : y : gs)
    -- go (Delay g) = go g
    getInterp :: State ([S], Env.Env) VI.Interpretation
    getInterp = do
      Env.Env _ i _ <- gets snd
      return i

postEval :: [X] -> G X -> G X
postEval as goal =
    let freshs = fv goal \\ as in
    foldr Fresh (go (freshs ++ as) goal) freshs
  where
    go vars (Conjunction x y gs) = unsafeConj $ go vars <$> (x : y : gs)
    go vars (Disjunction x y gs) = unsafeDisj $ go vars <$> (x : y : gs)
    go _ g = g

closeFresh :: [X] -> G X -> G X
closeFresh as goal = goal
  --   let goalNoFresh = stripFresh goal in
  --   -- let (f, xs) = go as goal in
  --   let (f, xs) = go as goalNoFresh in
  --   let res = f [] in
  --   res
  -- where
  --   go as (g :/\: h) =
  --     let (f1, uv1) = go as g in
  --     let (f2, uv2) = go as h in
  --     let uv = union uv1 uv2 in
  --     let func = (\dv ->
  --                     let udv = uv \\ dv in
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

  --   getFresh goal as = fv goal \\ as

topLevel :: Program G X -> Stream (Subst.Subst, FN.FreshNames)
topLevel (Program defs goal) =
  let env = Env.updateDefs Env.empty defs in
  let ((goal', _), env') = runState (preEval goal) env in
  eval env' Subst.empty goal'

-- Evaluation relation
eval :: Env.Env -> Subst.Subst -> G S -> Stream (Subst.Subst, FN.FreshNames)
eval env s (t1 :=:  t2) = fmap (, Env.getFreshNames env) (maybeToStream $ unify (Just s) t1 t2)
-- eval env s (g1 :\/: g2) = eval env s g1 `mplus` eval env s g2
eval env s (Disjunction x y gs) =
  foldr1 mplus (eval env s <$> (x : y : gs))
-- eval env s (g1 :/\: g2) = eval env s g1 >>= (\ (s', d') -> eval (Env.updateNames env d') s' g2)
eval env s (Conjunction x y gs) =
  eval env s x >>= \(s', d') ->
  eval (Env.updateNames env d') s' (unsafeConj $ y : gs)
eval env s (Invoke f as) =
  let (Def _ fs g) = Env.getDef env f in
  let i' = foldl (\ i'' (f', a) -> VI.extend i'' f' a) (Env.getInterp env) $ zip fs as in
  let ((g', _), env') = runState (preEval g) (Env.updateInterp env i') in
  eval env' s g'
-- eval env s (Delay goal) =
--   Immature (eval env s goal)
eval _ _ _ = error "Impossible case in eval"

run :: Program G X -> Stream Subst.Subst
run (Program defs goal) =
  let env = Env.fromDefs defs in
  let ((goal',_), env') = runState (preEval goal) env in
  fst <$> eval env' Subst.empty goal'
