{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module TaglessFinal.Syntax where

import           Control.Monad.State
import qualified Stream
import qualified TaglessFinal.Subst    as Subst
import           TaglessFinal.Term
import           TaglessFinal.VarState

type T = Term Var

type G = State VarState (Subst.Subst Var -> Stream.Stream (Subst.Subst Var))

class Goal repr where
  term :: T -> repr T
  unify :: repr T -> repr T -> repr G
  conj :: repr G -> repr G -> repr G
  disj :: repr G -> repr G -> repr G
  fresh :: (Var -> repr G) -> repr G

  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
  fixP :: String -> (repr a -> repr a) -> repr a

example1 :: Goal repr => Var -> repr G
example1 x = unify (term $ Con "Nil" []) (term $ Con "Cons" [Var x, Con "Nil" []])

example2 :: Goal repr => repr G
example2 = fresh $ \x -> fresh $ \y -> disj (conj (example1 x) (example1 x)) (example1 y)

unifyRelation :: Goal repr => repr (T -> T -> G)
unifyRelation = lam $ \x -> lam $ \y -> unify x y

unifyRelationApp :: Goal repr => repr G
unifyRelationApp = fresh $ \x -> fresh $ \y -> app (app unifyRelation (term (Var x))) (term (Var y))

f :: Goal repr => repr (Term Var -> a)
f = fixP "f" $ \self -> lam $ \x -> app self x

appendo :: Goal repr => repr (T -> T -> T -> G)
appendo =
  fixP "appendo" ( \self ->
    lam $ \x -> lam $ \y -> lam $ \z ->
      disj (conj (unify x (term $ Con "Nil" []))
                 (unify y z))
          (fresh $ \h -> fresh $ \t -> fresh $ \r ->
            (conj (unify x (term $ Con "Cons" [Var h, Var t]))
                  (conj (app (app (app self (term $ Var t)) y) (term $ Var r))
                        -- appendo (Var t) y (Var r)
                        (unify z (term $ Con "Cons" [Var h, Var r]))))
          )
  )

appendoApp :: Goal repr => repr G
appendoApp = fresh $ \x -> fresh $ \y -> fresh $ \z -> app (app (app appendo (term $ Var x)) (term $ Var y)) (term $ Var z)
