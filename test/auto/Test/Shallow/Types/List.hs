{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Shallow.Types.List(nil, cons) where
import Shallow.Core.Internal.Logic
import Shallow.Utils.Type

nil :: Logic [elem] var
nil = Ground Nil

cons :: Logic elem var -> Logic [elem] var -> Logic [elem] var
cons h t = Ground $ Cons h t

nil' :: Reified [elem] var
nil' = Nil

cons' :: Reified elem var -> Reified [elem] var -> Reified [elem] var
cons' h t = Cons (Ground h) (Ground t)

instance (LogicType a) => LogicType [a] where

    data Reified [a] var = Nil | Cons (Logic a var) (Logic [a] var)

    quote Nil = quote0 "Nil" Nil
    quote (Cons h t) = quote2 "Cons" Cons h t

    unifyVal _ Nil Nil = pure ()
    unifyVal unif (Cons h t) (Cons h' t') = unif h h' *> unif t t'
    unifyVal _ _ _ = empty

    derefVal _ Nil = pure []
    derefVal deref (Cons h t) = liftA2 (:) (deref h) (deref t)

    project [] = nil'
    project (x:xs) = cons' (project x) (project xs)

    reify Nil = return []
    reify (Cons (Ground h) (Ground t)) = liftA2 (:) (reify h) (reify t)
    reify (Cons _ _) = Nothing
