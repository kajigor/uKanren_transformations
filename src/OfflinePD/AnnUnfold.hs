module OfflinePD.AnnUnfold where

import           Control.Monad.State
import           Data.Maybe          (mapMaybe)
import           Text.Printf         (printf)
import           BTA.InvokeAnnotation
import           BTA.AnnotatedDef
import           Syntax (S, Term(..), X)
import           Util.ListZipper (Zipper)
import           BTA.NormalizeAnnotated          (fresh)
import qualified OfflinePD.AnnEnvironment as Env
import qualified VarInterpretation as VI
import qualified OfflinePD.AnnotatedEval as E
import qualified Eval
import qualified Subst

oneStepUnfold :: AnnG Term S -> State Env.Env (AnnG Term S)
oneStepUnfold g@(Invoke f as _) = do  --Unfold???
  env <- get
  let (AnnotatedDef n fs body _) = Env.getDef env f
  if length fs == length as
  then do
    let i' = foldl (\ interp (f, a) -> VI.extend interp f a) (Env.getInterp env) $ zip fs as
    let ((g', _), env') = runState (E.preEval body) (Env.updateInterp env i')
    put env'
    return g'
  else error $ printf "Unfolding error: different number of factual and actual arguments\nFactual: %s --- %s\nActual: %s --- %s)" f (show as) n (show fs)
oneStepUnfold g = return g

normalize :: AnnG Term S -> [[AnnG Term S]] -- disjunction of conjunctions of calls and unifications
normalize (Disjunction x y gs) = concatMap normalize (x : y : gs)
normalize (Conjunction x y gs) = (++) <$> normalize x <*> normalize (unsafeAnnConj (y : gs))
normalize g@Invoke {} = [[g]]
normalize g@(_ :=: _) = [[g]]
normalize (Delay g) = normalize g
normalize g = error ("Unexpected goal type in normalization\n" ++ show g)


unifyStuff :: Subst.Subst -> [AnnG Term S] -> Maybe ([AnnG Term S], Subst.Subst)
unifyStuff state gs =
    go gs state []
  where
    go [] state conjs = Just (reverse conjs, state)
    go (g@Invoke {} : gs) state conjs = go gs state (g : conjs)
    go ((t :=: u) : gs) state conjs = do
      s <- Eval.unify  (Just state) t u
      go gs s conjs


oneStep :: AnnG Term S -> Subst.Subst -> State Env.Env [([AnnG Term S], Subst.Subst)]
oneStep goal state = do
    unfolded <- oneStepUnfold goal
    let normalized = normalize unfolded
    let unified = mapMaybe (unifyStuff state) normalized
    return unified

maximumBranches :: AnnotatedDef (AnnG Term) X -> Int
maximumBranches def@(AnnotatedDef _ args body anns) =
    let goal = fst $ evalState (E.preEval (fresh args body)) Env.empty in
    length $ evalState (oneStep (succeed goal) Subst.empty) Env.empty
  where
    succeed (Invoke name args anns) = success
    succeed (t :=: u) = t :=: u
    succeed (Conjunction x y g) = unsafeAnnConj $ succeed <$> (x : y : g)
    succeed (Disjunction x y g) = unsafeAnnDisj $ succeed <$> (x : y : g)
    succeed (Fresh name g) = Fresh name $ succeed g
    success = C "" [] :=: C "" []

getMaximumBranches :: Env.Env -> AnnG Term S -> Int
getMaximumBranches env (Invoke name _ ann) =
    let def = Env.getDef env name in
    maximumBranches def