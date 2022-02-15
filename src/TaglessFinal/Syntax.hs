{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE FlexibleInstances #-}
module TaglessFinal.Syntax where

import TaglessFinal.Term
import qualified TaglessFinal.Subst as Subst
import qualified Stream
import qualified TaglessFinal.Unify as Unify
import TaglessFinal.VarState
import TaglessFinal.EvalState

import Control.Monad
import Control.Monad.State

class Goal repr where
  unify :: Term Var -> Term Var -> repr
  conj :: repr -> repr -> repr
  disj :: repr -> repr -> repr
  fresh :: (Var -> repr) -> repr

  lam :: (Term Var -> repr) -> repr
  app :: repr -> Term Var -> repr
  fixP :: String -> (repr -> repr) -> repr
  var :: String -> repr

appendo :: Goal repr => repr
appendo =
  lam $ \x -> lam $ \y -> lam $ \z ->
    fixP "appendo" ( \self ->
        disj (conj (unify x (Con "Nil" []))
                  (unify y z))
            (fresh $ \h -> fresh $ \t -> fresh $ \r ->
              (conj (unify x (Con "Cons" [Var h, Var t]))
                    (conj (app (app (app self (Var t)) y) (Var r))
                          -- appendo (Var t) y (Var r)
                          (unify z (Con "Cons" [Var h, Var r]))))
            )
    )

type EvalResult = Stream.Stream (Subst.Subst Var)

newtype Eval = Eval { runEval :: State EvalState EvalResult }

-- instance Goal Eval where
  -- unify t1 t2 = Eval $ return $ \s -> Stream.maybeToStream $ Unify.unify (Just s) t1 t2
  -- conj g1 g2 = Eval $ do
  --   state <- get
  --   return $ \s -> do
  --     let (f, state') = runState (runEval g1) state
  --     let (g, state'') = runState (runEval g2) state'
  --     f s >>= g
       --  >>= runEval g2
  -- disj g1 g2 = Eval $ \s -> runEval g1 s `mplus` runEval g2 s
  -- fresh goal =
  --   undefined

  -- lam = _
  -- app = _
  -- fixP = _
  -- var = _
