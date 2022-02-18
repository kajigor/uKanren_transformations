{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction#-}
module TaglessFinal.Syntax where

import TaglessFinal.Term
import qualified TaglessFinal.Subst as Subst
import qualified Stream
import qualified TaglessFinal.Unify as Unify
import TaglessFinal.VarState

import Control.Monad
import Control.Monad.State

type G = ()

class Goal repr where
  unify :: Term Var -> Term Var -> repr G
  conj :: repr G -> repr G -> repr G
  disj :: repr G -> repr G -> repr G
  fresh :: (Var -> repr G) -> repr G

  lam :: (Term Var -> repr a) -> repr (Term Var -> a)
  app :: repr (Term Var -> a) -> Term Var -> repr a
  fixP :: String -> (repr a -> repr a) -> repr a

example1 :: Goal repr => Var -> repr G
example1 x = unify (Con "Nil" []) (Con "Cons" [Var x, Con "Nil" []])

example2 :: Goal repr => repr G
example2 = fresh $ \x -> fresh $ \y -> disj (conj (example1 x) (example1 x)) (example1 y)

unifyRelation :: Goal repr => repr (Term Var -> Term Var -> G)
unifyRelation = lam $ \x -> lam $ \y -> unify x y

unifyRelationApp :: Goal repr => repr G
unifyRelationApp = fresh $ \x -> fresh $ \y -> app (app unifyRelation (Var x)) (Var y)

f :: Goal repr => repr (Term Var -> a)
f = fixP "f" $ \self -> lam $ \x -> app self x



appendo =
  fixP "appendo" ( \self ->
    lam $ \x -> lam $ \y -> lam $ \z ->
      disj (conj (unify x (Con "Nil" []))
                (unify y z))
          (fresh $ \h -> fresh $ \t -> fresh $ \r ->
            (conj (unify x (Con "Cons" [Var h, Var t]))
                  (conj (app (app (app self (Var t)) y) (Var r))
                        -- appendo (Var t) y (Var r)
                        (unify z (Con "Cons" [Var h, Var r]))))
          )
  )

-- type EvalResult = Stream.Stream (Subst.Subst Var)

-- newtype Eval = Eval { runEval :: State EvalState EvalResult }

newtype Eval a = Eval { runEval :: State VarState (Subst.Subst Var -> Stream.Stream (Subst.Subst Var))}

-- instance Goal Eval where
--   unify t1 t2 = Eval $ do
--     return $ \s -> Stream.maybeToStream $ Unify.unify (Just s) t1 t2

--   conj g1 g2 = Eval $ do
--     g1' <- runEval g1
--     g2' <- runEval g2
--     return $ \s -> g1' s >>= g2'

--   disj g1 g2 = Eval $ do
--     g1' <- runEval g1
--     g2' <- runEval g2
--     return $ \s -> g1' s `mplus` g2' s

--   fresh f = Eval $ do
--     v <- toFreshVar <$> nextFreshVar
--     runEval (f v)

--   lam f = Eval $ do
--     v <- Var <$> toVar <$> nextVar
--     runEval (f v)

--   app f x = Eval $ do
--     f' <- runEval f
--     return $ f' x


  -- app = _
  -- fixP = _
