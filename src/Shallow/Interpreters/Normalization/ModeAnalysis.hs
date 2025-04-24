{-# LANGUAGE GADTs, KindSignatures, DeriveFunctor, StandaloneDeriving, GeneralisedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances, TypeFamilies, Rank2Types, MultiParamTypeClasses, FlexibleContexts #-}
module Shallow.Interpreters.Normalization.ModeAnalysis where

import Shallow.Core.Internal.Kanren
import Shallow.Utils.Kanren
import Data.Kind (Type)
import Shallow.Core.Logic
import Control.Applicative
import Shallow.Utils.Logic (vmap, vmapM_)
import Control.Monad.State
import Shallow.Interpreters.Normalization.Normalize
import Data.Foldable (traverse_)
import Shallow.Core.Internal.Logic (Field(..), LogicType (..))

type K rel = NormalizedKanrenT rel

data Mode = In | Out deriving (Eq, Show)
data ModeState rel = ModeState { getMode :: forall a. KVar (K rel) a -> Mode }

fullyFree :: ModeState rel
fullyFree = ModeState $ const Out

setMode :: (EqVar rel) => KVar (K rel) a -> Mode -> ModeState rel -> ModeState rel
setMode v m s = s { getMode = \v' -> if v `varEq` v' then m else getMode s v }

updateMode :: (EqVar rel) => NormalizedBaseT rel -> State (ModeState rel) ()
updateMode (Unify v x) = do
    modify $ setMode v In
    vmapM_ (\v' -> modify $ setMode v' In) x
    pure ()
updateMode (Call _ _ r) = helper r
    where
        helper :: (EqVar rel) => NormalizedFreshT rel a -> State (ModeState rel) ()
        helper (FreshDone _) = pure ()
        helper (Fresh FreshVar f) = helper (f (error "Accessed variable during mode analysis")) 
        helper (Fresh (ArgVar x) f) = do
            vmapM_ (\v' -> modify $ setMode v' In) x
            helper $ f (error "Accessed variable during mode analysis")

isFullyGround :: (LogicType a, EqVar rel) => L a (K rel) -> ModeState rel -> Bool
isFullyGround (Free v) s = getMode s v == In
isFullyGround (Ground x) s = let (_, elems) = quote x in all (\(Field _ x') -> isFullyGround x' s) elems

class (EqVar (MRel t), Kanren (MRel t), Monoid (Result t)) => ModeKanren t where

    type (MRel t) :: Type -> Type
    data (Result t) :: Type

    modedFresh :: (LogicType x) => FreshType rel x -> (Var' x (K rel) -> [Result t]) -> [Result t]

    modedUnify :: (LogicType a) => ModeState rel -> Var' a (K rel) -> L a (K rel) -> Result t

    modedCall :: ModeState rel -> CallType -> Relation (K rel) -> Result t


modeAnalysis :: (ModeKanren t) => NormalizedKanrenT (MRel t) a -> ModeState (MRel t) -> [Result t]
modeAnalysis = analyseFresh

analyseFresh :: (ModeKanren t) => NormalizedFreshT (MRel t) a -> ModeState (MRel t) -> [Result t]
analyseFresh (FreshDone r) s = analyseDisj r s
analyseFresh (Fresh FreshVar g) s = modedFresh FreshVar $ 
    \v -> analyseFresh (g v) (setMode v Out s)
analyseFresh (Fresh (ArgVar x) g) s = modedFresh (ArgVar $ vmap restoreVar x) $ 
    \v -> analyseFresh (g v) (setMode v argMode s)
    where
        argMode | isFullyGround x s = In
                | otherwise = Out

analyseDisj :: (ModeKanren t) => NormalizedDisjT (MRel t) a -> ModeState (MRel t) -> [Result t]
analyseDisj (Disj gs) s = map (\g -> evalState (analyseConj (goal g)) s) gs

analyseConj :: (ModeKanren t) => NormalizedConjT (MRel t) -> State (ModeState (MRel t)) (Result t)
analyseConj (Conj []) = pure mempty
analyseConj (Conj (g:gs)) = do
    s <- get
    let g' = analyseBase g s
    updateMode g
    gs' <- analyseConj (Conj gs)
    return $ g' <> gs'

analyseBase :: (ModeKanren t) => NormalizedBaseT (MRel t) -> ModeState (MRel t) -> Result t
analyseBase (Unify v x) s = modedUnify s v x
analyseBase (Call t n r) s = modedCall s t (Relation n r)

