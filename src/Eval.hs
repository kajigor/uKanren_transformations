{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleContexts #-}

module Eval where

import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Def
import qualified Environment         as Env
import qualified FreshNames          as FN
import           Program
import           Stream
import qualified Subst
import           Syntax
import qualified VarInterpretation   as VI

unifyG :: (Ord v) => (Subst.Subst v -> v -> Term v -> Bool)
          -> Maybe (Subst.Subst v) -> Term v -> Term v -> Maybe (Subst.Subst v)
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

walk :: (Ord v) => Subst.Subst v -> Term v -> Term v
walk s x@(V v) =
  maybe x (walk s) $ Subst.lookup v s
walk _ u = u

-- Unification
unify :: (Ord v) => Maybe (Subst.Subst v) -> Term v -> Term v -> Maybe (Subst.Subst v)
unify =
    unifyG occursCheck
  where
    occursCheck :: (Ord v) => Subst.Subst v -> v -> Term v -> Bool
    occursCheck s u t =
      case walk s t of
        V v -> v == u
        C _ as -> any (occursCheck s u) as

    -- occursCheck u' t s = if elem u' $ fv t then Nothing else s

unifyAll :: (Ord v) => Maybe (Subst.Subst v) -> [(Term v, Term v)] -> Maybe (Subst.Subst v)
unifyAll = foldl (\s (x, y) -> unify s x y)

unifySubsts :: Subst.Subst S -> Subst.Subst S -> Maybe (Subst.Subst S)
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

unifyNoOccursCheck :: (Ord v) => Maybe (Subst.Subst v) -> Term v -> Term v -> Maybe (Subst.Subst v)
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
    go (Delay g) = Delay <$> go g
    getInterp :: State ([S], Env.Env) VI.Interpretation
    getInterp = do
      Env.Env _ i _ <- gets snd
      return i

bindFresh :: (FreeVariables G a, Ord a) => [a] -> G a -> G a
bindFresh bound = go (Set.fromList bound)
  where
    go bound (Disjunction x y gs) =
      Disjunction (go bound x)
                  (go bound y)
                  (map (go bound) gs)
    go bound g@(Conjunction x y gs) =
      fresh (fv g \\ Set.toList bound) g
    go bound (Fresh x g) = 
      Fresh x (go (Set.insert x bound) g)
    go bound (Delay g) = 
      Delay (go bound g)
    go bound t@(_ :=: _) = 
      fresh (fv t \\ Set.toList bound) t 



-- data G a
--   = Term a :=: Term a
--   | Conjunction (G a) (G a) [G a] -- a list of conjuncts: at least 2 conjuncts should be present
--   | Disjunction (G a) (G a) [G a] -- a list of disjuncts: at least 2 disjuncts should be present
--   | Fresh a (G a)
--   | Invoke Name [Term a]
--   | Delay (G a)
--   deriving (Eq, Ord, Functor)

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

topLevel :: Program G X -> Stream (Subst.Subst S, FN.FreshNames)
topLevel (Program defs goal) =
  let env = Env.updateDefs Env.empty defs in
  let ((goal', _), env') = runState (preEval goal) env in
  eval env' Subst.empty goal'

-- Evaluation relation
eval :: Env.Env -> Subst.Subst S -> G S -> Stream (Subst.Subst S, FN.FreshNames)
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
eval env s (Delay goal) =
  Immature (eval env s goal)
eval _ _ _ = error "Impossible case in eval"

run :: Program G X -> Stream (Subst.Subst S)
run (Program defs goal) =
  let env = Env.fromDefs defs in
  let ((goal',_), env') = runState (preEval goal) env in
  fst <$> eval env' Subst.empty goal'