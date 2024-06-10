module OfflinePD.AnnotatedEval where

import           BTA.InvokeAnnotation 
import           Control.Monad.State
import           Syntax (X, S, Term(..))
import           BTA.NormalizeAnnotated (unsafeConj, unsafeDisj)
import qualified OfflinePD.AnnEnvironment as Env
import qualified VarInterpretation as VI
import qualified FreshNames as FN


preEval :: AnnG Term X -> State Env.Env (AnnG Term S, [S])
preEval goal = do
    env <- get
    let (g, (vars, env')) = runState (go goal) ([], env)
    put env'
    return (g, vars)
  where
    go :: AnnG Term X -> State ([S], Env.Env) (AnnG Term S)
    go (Fresh x g') = do
      (vars, Env.Env p i d) <- get
      let (y, d') = FN.getFreshName d
      put (y:vars, Env.Env p (VI.extend i x (V y)) d')
      go g'
    go (t1 :=: t2) = do
      i <- getInterp
      return ((i VI.<@> t1) :=: (i VI.<@> t2))
    go (Invoke f fs ann) = do
      i <- getInterp
      return (Invoke f (map (i VI.<@>) fs) ann)
    go (Conjunction x y gs) = unsafeConj <$> mapM go (x : y : gs)
    go (Disjunction x y gs) = unsafeDisj <$> mapM go (x : y : gs)
    go (Delay g) = Delay <$> go g
    getInterp :: State ([S], Env.Env) VI.Interpretation
    getInterp = do
      Env.Env _ i _ <- gets snd
      return i
      
