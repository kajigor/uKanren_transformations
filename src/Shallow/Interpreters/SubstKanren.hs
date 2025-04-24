{-# LANGUAGE TypeFamilies, DeriveFunctor, Rank2Types, GeneralisedNewtypeDeriving, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Shallow.Interpreters.SubstKanren
(
  runSubstKanren
, Stream
, takeS
, takeWhileS
) where

import Shallow.Core.Internal.Kanren
import Stream
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)
import Shallow.Utils.Kanren
import Debug.Trace
import Shallow.Utils.Logic (showLogic)

type VarRepr = Int
type V s t = KVar (SubstKanren s) t

data Subst s = Subst { subst :: forall t. V s t -> Maybe t, nextVar :: VarRepr }

emptySubst :: Subst s
emptySubst = Subst { subst = \v -> error $ "Invalid variable " ++ displayVar v, nextVar = toEnum 0 }

readSubst :: V s a -> Subst s -> Maybe a
readSubst v s = subst s v

updateSubst :: V s a -> Maybe a -> Subst s -> Subst s
updateSubst v a s = s { subst = \v' -> if v `varEq` v' then unsafeCoerce a else subst s v' }

succVar :: Subst s -> Subst s
succVar s = s { nextVar = succ (nextVar s) }

newtype SubstKanren s a = SubstKanren (StateT (Subst s) Stream a) deriving (Functor, Applicative, Alternative, Monad)
type R s = SubstKanren s

instance MonadState (Subst s) (R s) where
    state = SubstKanren . state

readVar :: V s t -> R s (Maybe t)
readVar v = gets $ readSubst v

makeVar :: Maybe (L a (R s)) -> R s (Var' a (R s))
makeVar x = do
    v <- SVar <$> gets nextVar
    modify $ succVar . updateSubst v x
    return $ v

instance Kanren (R s) where

    newtype instance (KVar (R s)) t = SVar VarRepr deriving (Functor, Show)

    fresh_ FreshVar   = (makeVar Nothing >>=)
    fresh_ (ArgVar x) = (makeVar (Just x) >>=)
    unify = flatteningUnify unif
        where
            unif v y | occursCheck v y = empty
                     | otherwise = do
                        x <- readVar v
                        maybe (modify $ updateSubst v (Just y)) (unify y) x
    call_ Opaque (Relation _ (SubstKanren r)) = SubstKanren $ mapStateT Immature r
    call_ Transparent (Relation _ r) = r
    displayVar (SVar v) = "x" ++ show v

instance KanrenEval (R s) where

    derefVar v = do
        x <- gets $ readSubst v
        maybe (error $ "Unbound variable: " ++ displayVar v) eval x

instance EqVar (R s) where 
    varEq (SVar a) (SVar b) = a == b


runSubstKanren :: (forall s. R s t) -> Stream t 
runSubstKanren (SubstKanren r) = evalStateT r emptySubst