{-# LANGUAGE GADTs, TypeFamilies, KindSignatures, StandaloneDeriving, FlexibleContexts, UndecidableInstances, Rank2Types, ApplicativeDo, MultiParamTypeClasses, QuantifiedConstraints #-}
module Shallow.Core.Internal.Logic(module Shallow.Core.Internal.Logic) where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Control.Applicative

data Field a var where
  Field :: LogicType x => Proxy x -> Logic x var -> Field a var

data Constructor a = Constructor
  { name :: String
  , construct :: forall var. [Field a var] -> Reified a var
  }

type Var x (var :: Type -> Type) = var (Logic x var)
data Logic a (var :: Type -> Type) 
  = Free (Var a var) | Ground (Reified a var)

class LogicType a where

  data Reified a (var :: Type -> Type) :: Type
  project :: a -> Reified a var
  reify :: Reified a var -> Maybe a
  unifyVal :: (Alternative rel) => (forall t. LogicType t => Logic t var -> Logic t var -> rel ()) -> Reified a var -> Reified a var -> rel ()
  quote :: Reified a var -> (Constructor a, [Field a var])
  derefVal :: (Alternative rel) => (forall t. LogicType t => Logic t var -> rel t) -> Reified a var -> rel a