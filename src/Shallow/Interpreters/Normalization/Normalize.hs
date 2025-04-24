{-# LANGUAGE GADTs, KindSignatures, DeriveFunctor, StandaloneDeriving, FlexibleContexts, GeneralisedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances, TypeFamilies #-}
module Shallow.Interpreters.Normalization.Normalize (
    NormalizedBaseT(..),
    NormalizedConjT(..),
    NormalizedRaiseT(..),
    NormalizedDisjT(..),
    NormalizedFreshT(..),
    NormalizedKanrenT,
    NormalizedKanren(..),
    wrapVar, restoreVar,
    normalize, normalizeRelation,
    interpretNorm, interpretNormRelation
) where

import Shallow.Core.Internal.Kanren
import Shallow.Utils.Kanren
import Data.Kind (Type)
import Shallow.Core.Logic
import Control.Applicative
import Data.Foldable (traverse_)
import Shallow.Utils.Logic (vmap)
import Data.Functor (($>))
import Debug.Trace 

data NormalizedBaseT (rel :: Type -> Type) where

    Unify :: (LogicType a) => Var' a (NormalizedKanrenT rel) -> L a (NormalizedKanrenT rel) -> NormalizedBaseT rel
    Call :: CallType -> String -> NormalizedKanrenT rel () -> NormalizedBaseT rel

newtype NormalizedConjT rel = Conj { unConj :: [NormalizedBaseT rel] }

conj :: [NormalizedBaseT rel] -> NormalizedConjT rel
conj = Conj

data NormalizedRaiseT rel a = Raise { goal :: NormalizedConjT rel, retval :: a } deriving (Functor)

raise :: NormalizedConjT rel -> a -> NormalizedRaiseT rel a
raise = Raise

newtype NormalizedDisjT rel a = Disj { unDisj :: [NormalizedRaiseT rel a] } deriving (Functor)

disj :: [NormalizedRaiseT rel a] -> NormalizedDisjT rel a
disj = Disj

data NormalizedFreshT rel a where

    FreshDone :: NormalizedDisjT rel a -> NormalizedFreshT rel a
    Fresh :: (LogicType x) => FreshType (NormalizedKanrenT rel) x -> (Var' x (NormalizedKanrenT rel) -> NormalizedFreshT rel a) -> NormalizedFreshT rel a

type NormalizedKanrenT rel = NormalizedFreshT rel

instance Applicative (NormalizedRaiseT rel) where

    pure = raise (conj [])

    (Raise g f) <*> (Raise g' x) = raise (conj $ unConj g ++ unConj g') (f x)

instance Applicative (NormalizedDisjT rel) where

    pure a = disj [pure a]

    (Disj gs) <*> (Disj gs') = disj [g <*> g' | g <- gs, g' <- gs']

instance Alternative (NormalizedDisjT rel) where

    empty = disj []

    (Disj gs) <|> (Disj gs') = disj $ gs ++ gs'

deriving instance Functor (NormalizedFreshT rel)

instance Applicative (NormalizedFreshT rel) where

    pure = FreshDone . pure

    (FreshDone g) <*> (FreshDone g') = FreshDone $ g <*> g'
    (Fresh x@(ArgVar _) g) <*> g' = Fresh x $ \v -> g v <*> g'
    g <*> (Fresh x@(ArgVar _) g') = Fresh x $ \v -> g <*> g' v
    (Fresh x g) <*> g' = Fresh x $ \v -> g v <*> g'
    g <*> (Fresh x g') = Fresh x $ \v -> g <*> g' v

instance Alternative (NormalizedFreshT rel) where

    empty = FreshDone empty

    (FreshDone g) <|> (FreshDone g') = FreshDone $ g <|> g'
    (Fresh x@(ArgVar _) g) <|> g' = Fresh x $ \v -> g v <|> g'
    g <|> (Fresh x@(ArgVar _) g') = Fresh x $ \v -> g <|> g' v
    (Fresh x g) <|> g' = Fresh x $ \v -> g v <|> g'
    g <|> (Fresh x g') = Fresh x $ \v -> g <|> g' v


instance (Kanren rel) => Kanren (NormalizedKanrenT rel) where

    newtype instance (KVar (NormalizedKanrenT rel)) a = NV (KVar rel a)

    fresh_ = Fresh
    unify = flatteningUnify (\x t -> FreshDone $ disj [raise (conj [Unify x t]) ()]) -- TODO: Linearize unifications

    call_ t (Relation s r) = FreshDone $ disj [raise (conj [Call t s r]) ()]

    displayVar (NV v) = displayVar v

instance (Kanren rel) => Functor (KVar (NormalizedKanrenT rel)) where

    fmap f (NV x) = NV $ fmap f x

instance (EqVar rel) => EqVar (NormalizedKanrenT rel) where

    varEq (NV x) (NV y) = varEq x y

restoreVar :: (Kanren rel, LogicType a) => Var' a (NormalizedKanrenT rel) -> Var' a rel
restoreVar (NV v) = (vmap restoreVar) <$> v

wrapVar :: (Kanren rel, LogicType a) => Var' a rel -> Var' a (NormalizedKanrenT rel)
wrapVar v = NV $ vmap wrapVar <$> v

restoreFreshType :: (Kanren rel, LogicType a) => FreshType (NormalizedKanrenT rel) a -> FreshType rel a
restoreFreshType FreshVar = FreshVar
restoreFreshType (ArgVar x) = ArgVar $ vmap restoreVar x

restoreBase :: (Kanren rel) => NormalizedBaseT rel -> rel ()
restoreBase (Unify v x) = unify (Free $ restoreVar v) (vmap restoreVar x)
restoreBase (Call t s r) = call_ t (Relation s $ normalize r)

restoreConj :: (Kanren rel) => NormalizedConjT rel -> rel ()
restoreConj (Conj gs) = traverse_ restoreBase gs

restoreRaise :: (Kanren rel) => NormalizedRaiseT rel a -> rel a
restoreRaise (Raise g x) = restoreConj g $> x

restoreDisj :: (Kanren rel) => NormalizedDisjT rel a -> rel a
restoreDisj (Disj gs) = asum $ restoreRaise <$> gs

restoreFresh :: (Kanren rel) => NormalizedFreshT rel a -> rel a
restoreFresh (FreshDone g) = restoreDisj g
restoreFresh (Fresh x f) = fresh_ (restoreFreshType x) $ restoreFresh <$> (f . wrapVar)

normalize :: (Kanren rel) => NormalizedKanrenT rel a -> rel a
normalize = restoreFresh

normalizeRelation :: (Kanren rel) => Relation (NormalizedKanrenT rel) -> Relation rel
normalizeRelation (Relation s r) = Relation s (normalize r)


class (Kanren (UnderlyingRel rel)) => NormalizedKanren (rel :: Type -> Type) where

    type UnderlyingRel rel :: Type -> Type

    data NBase rel :: Type
    data NConj rel :: Type
    data NRaise rel :: Type -> Type
    data NDisj rel :: Type -> Type

    unifyVarNorm :: (LogicType a) => Var' a (UnderlyingRel rel) -> L a (UnderlyingRel rel) -> NBase rel

    callNorm_ :: CallType -> Relation rel -> NBase rel

    freshNorm_ :: (LogicType a) => FreshType (UnderlyingRel rel) a -> (Var' a (UnderlyingRel rel) -> rel x) -> rel x

    liftBase :: NBase rel -> NConj rel
    liftConj :: NConj rel -> a -> NRaise rel a
    liftRaise :: NRaise rel a -> NDisj rel a
    liftDisj :: NDisj rel a -> rel a
    
    makeConj :: [NBase rel] -> NConj rel
    makeDisj :: [NRaise rel a] -> NDisj rel a


instance (Kanren rel) => NormalizedKanren (NormalizedKanrenT rel) where

    type instance UnderlyingRel (NormalizedKanrenT rel) = rel

    newtype instance NBase (NormalizedKanrenT rel) = NKB (NormalizedBaseT rel)
    newtype instance NConj (NormalizedKanrenT rel) = NKC (NormalizedConjT rel)
    newtype instance NRaise (NormalizedKanrenT rel) a = NKR (NormalizedRaiseT rel a) deriving (Functor, Applicative)
    newtype instance NDisj (NormalizedKanrenT rel) a = NKD (NormalizedDisjT rel a) deriving (Functor, Applicative, Alternative)

    unifyVarNorm a b = NKB $ Unify (wrapVar a) (vmap wrapVar b)

    callNorm_ t (Relation n r) = NKB $ Call t n r

    freshNorm_ FreshVar f = Fresh FreshVar $ f . restoreVar
    freshNorm_ (ArgVar x) f = Fresh (ArgVar $ vmap wrapVar x) $ f . restoreVar

    liftBase (NKB r) = NKC $ conj [r]
    liftConj (NKC r) a = NKR $ raise r a
    liftRaise (NKR r) = NKD $ disj [r]
    liftDisj (NKD r) = FreshDone r

    makeConj gs = NKC $ conj $ (\(NKB b) -> b) <$> gs
    makeDisj gs = NKD $ disj $ (\(NKR r) -> r) <$> gs

interpretNormBase :: (NormalizedKanren nrel) => NormalizedBaseT (UnderlyingRel nrel) -> NBase nrel
interpretNormBase (Unify v x) = unifyVarNorm (restoreVar v) (vmap restoreVar x)
interpretNormBase (Call t s r) = callNorm_ t $ interpretNormRelation (Relation s r)

interpretNormConj :: (NormalizedKanren nrel) => NormalizedConjT (UnderlyingRel nrel) -> NConj nrel
interpretNormConj (Conj gs) = let gs' = interpretNormBase <$> gs in makeConj gs'

interpretNormRaise :: (NormalizedKanren nrel) => NormalizedRaiseT (UnderlyingRel nrel) a -> NRaise nrel a
interpretNormRaise (Raise g x) = liftConj (interpretNormConj g) x

interpretNormDisj :: (NormalizedKanren nrel) => NormalizedDisjT (UnderlyingRel nrel) a -> NDisj nrel a
interpretNormDisj (Disj gs) = let gs' = interpretNormRaise <$> gs in makeDisj gs'

interpretNormFresh :: (NormalizedKanren nrel) => NormalizedFreshT (UnderlyingRel nrel) a -> nrel a
interpretNormFresh (FreshDone g) = liftDisj $ interpretNormDisj g
interpretNormFresh (Fresh x f) = freshNorm_ (restoreFreshType x) $ interpretNormFresh <$> (f . wrapVar)

interpretNorm :: (NormalizedKanren nrel) => NormalizedKanrenT (UnderlyingRel nrel) a -> nrel a
interpretNorm = interpretNormFresh

interpretNormRelation :: (NormalizedKanren nrel) => Relation (NormalizedKanrenT (UnderlyingRel nrel)) -> Relation nrel
interpretNormRelation (Relation s r) = Relation s (interpretNorm r)