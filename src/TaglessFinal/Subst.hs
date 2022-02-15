{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TaglessFinal.Subst where

import TaglessFinal.Term
import qualified Data.Map.Strict as Map
import Text.Printf ( printf )
import Data.List ( intercalate )
import Prelude hiding ( lookup, null )

newtype Subst a = Subst { getSubst :: Map.Map a (Term a) }
                deriving Eq

empty :: Subst a
empty = Subst Map.empty

lookup :: Ord a => a -> Subst a -> Maybe (Term a)
lookup k = Map.lookup k . getSubst

insert :: Ord a => a -> Term a -> Subst a -> Subst a
insert k v (Subst s) = Subst $ Map.insert k v s

size :: Subst a -> Int
size = Map.size . getSubst

difference :: Ord a => Subst a -> Subst a -> Subst a
difference (Subst x) (Subst y) = Subst $ Map.difference x y

intersection :: Ord a => Subst a -> Subst a -> Subst a
intersection (Subst x) (Subst y) = Subst $ Map.intersection x y

intersectionWith :: Ord a => (Term a -> Term a -> b) -> Subst a -> Subst a -> Map.Map a b
intersectionWith f (Subst x) (Subst y) = Map.intersectionWith f x y

toList :: Subst a -> [(a, Term a)]
toList = Map.toList . getSubst

null :: Subst a -> Bool
null = Map.null . getSubst

fromList :: Ord a => [(a, Term a)] -> Subst a
fromList = Subst . Map.fromList

-- Applying substitution
class ApplySubst c where
  substitute :: (Show a, Ord a) => Subst a -> c a -> c a

instance ApplySubst Term where
  substitute s t@(Var x) =
    case lookup x s of
      Just tx | tx /= t -> substitute s tx
      _                 -> t
  substitute s (Con m ts) = Con m $ map (substitute s) ts

---- Composing substitutions
o :: (Ord a, Show a) => Subst a -> Subst a -> Subst a
o sigma theta =
  if null $ intersection sigma theta
  then
    Subst $ Map.fromList $ map (\ (s, ts) -> (s, substitute sigma ts)) (toList theta) ++ toList sigma
  else
    error "Non-disjoint domains in substitution composition"

instance Show a => Show (Subst a) where
  show (Subst s) = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (show $ Var x) (show y)) (Map.toList s)))

showSubst' :: Show a => Subst a -> String
showSubst' s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s -> %s" (show $ Var x) (show y)) (Map.toList (getSubst s))))


