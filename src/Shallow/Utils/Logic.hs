{-# LANGUAGE Rank2Types, EmptyDataDeriving, FlexibleContexts, UndecidableInstances #-}
module Shallow.Utils.Logic
(
  NoVars
, vmapM, vmapM_
, vmapMVal, vmapMVal_
, vmap
, showLogic
, reify', project'
) where
import Shallow.Core.Internal.Logic
import Shallow.Core.Internal.Kanren
import Control.Monad.Identity (Identity(runIdentity, Identity))

data NoVars t deriving (Show, Eq)

vmapM :: (LogicType a, Monad m) => (forall x. (LogicType x) => Var x var -> m (Var x var')) -> Logic a var -> m (Logic a var')
vmapM f (Free v) = Free <$> f v
vmapM f (Ground x) = Ground <$> f `vmapMVal` x

vmapM_ :: (LogicType a, Monad m) => (forall x. (LogicType x) => Var x var -> m ()) -> Logic a var -> m ()
vmapM_ f (Free v) = f v
vmapM_ f (Ground x) = f `vmapMVal_` x

vmapMVal :: (LogicType a, Monad m) => (forall x. (LogicType x) => Var x var -> m (Var x var')) -> Reified a var -> m (Reified a var')
vmapMVal f x = let (con, elems) = quote x in construct con <$> (mapM (\(Field p x') -> Field p <$> f `vmapM` x') elems)

vmapMVal_ :: (LogicType a, Monad m) => (forall x. (LogicType x) => Var x var -> m ()) -> Reified a var -> m ()
vmapMVal_ f x = let (_, elems) = quote x in (mapM_ (\(Field _ x') -> f `vmapM_` x') elems)

vmap :: (LogicType a) => (forall x. (LogicType x) => Var x var -> Var x var') -> Logic a var -> Logic a var'
vmap f x = runIdentity $ vmapM (Identity . f) x

showLogic :: (Show (Reified a var)) => Logic a var -> String
showLogic (Free v) = "v"
showLogic (Ground x) = show x

project' :: (LogicType a) => a -> Logic a var
project' = Ground . project

reify' :: (LogicType a) => Logic a var -> Maybe a
reify' (Ground x) = reify x
reify' _ = Nothing