{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}
module Mode.Term where

import           Data.List   (intercalate)
import qualified Data.Set    as Set
import           Text.Printf
import qualified Syntax as S


newtype Var a = Var { getVar :: a }
              deriving (Eq, Ord, Functor)

instance Foldable Var where 
  foldMap f (Var v) = f v

instance Traversable Var where 
  traverse f (Var v) = Var <$> f v 

instance Show a => Show (Var a) where
  show (Var x) = printf "V.%s" $ show x
  showList xs =
      showChar '(' . (go xs ++) . showChar ')'
    where
      go xs = intercalate ", " (map show xs)

data FlatTerm a = FTCon String [Var a]
                | FTVar (Var a)
                deriving (Eq, Ord, Functor)

instance Foldable FlatTerm where 
  foldMap f (FTVar v) = foldMap f v 
  foldMap f (FTCon _ vars) = foldMap (foldMap f) vars

instance Traversable FlatTerm where 
  traverse f (FTVar v) = FTVar <$> traverse f v 
  traverse f (FTCon name vars) = FTCon name <$> traverse (traverse f) vars 

instance Show a => Show (FlatTerm a) where
  show (FTCon name args) = printf "%s %s" name $ show args
  show (FTVar v) = show v

varsFromTerm :: Ord a => FlatTerm a -> Set.Set a
varsFromTerm (FTVar v) = Set.singleton $ getVar v
varsFromTerm (FTCon _ args) = Set.fromList $ map getVar args

varToPlainTerm :: Var a -> S.Term a
varToPlainTerm (Var v) = S.V v

toPlainTerm :: FlatTerm a -> S.Term a
toPlainTerm (FTVar v) = varToPlainTerm v
toPlainTerm (FTCon n args) = S.C n (varToPlainTerm <$> args)