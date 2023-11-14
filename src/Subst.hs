{-# LANGUAGE FlexibleInstances #-}

module Subst where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import           Prelude         hiding (lookup, null)
import           Syntax
import           Text.Printf     (printf)

newtype Subst v = Subst { getSubst :: Map.Map v (Term v) }
              deriving Eq

empty :: Subst v
empty = Subst Map.empty

lookup :: (Ord v) => v -> Subst v -> Maybe (Term v)
lookup k = Map.lookup k . getSubst

insert :: (Ord v) => v -> Term v -> Subst v -> Subst v
insert k v (Subst s) = Subst $ Map.insert k v s

size :: Subst v -> Int
size = Map.size . getSubst

difference :: (Ord v) => Subst v -> Subst v -> Subst v
difference (Subst x) (Subst y) = Subst $ Map.difference x y

intersection :: (Ord v) => Subst v -> Subst v -> Subst v
intersection (Subst x) (Subst y) = Subst $ Map.intersection x y

intersectionWith :: (Ord v) => (Term v -> Term v -> b) -> Subst v -> Subst v -> Map.Map v b
intersectionWith f (Subst x) (Subst y) = Map.intersectionWith f x y

toList :: Subst v -> [(v, Term v)]
toList = Map.toList . getSubst

null :: Subst v -> Bool
null = Map.null . getSubst

fromList :: (Ord v) => [(v, Term v)] -> Subst v
fromList = Subst . Map.fromList

-- This is wacky: it may clash vars
union :: (Ord v) => Subst v -> Subst v -> Subst v
union (Subst s) (Subst s') = Subst $ Map.union s s'

-- Applying substitution
class ApplySubst a where
  substitute :: (Ord v) => Subst v -> a v -> a v

instance ApplySubst Term where
  substitute s t@(V x) =
    case lookup x s of
      Just tx | tx /= t -> substitute s tx
      _                 -> t
  substitute s (C m ts) = C m $ map (substitute s) ts

instance ApplySubst G where
  substitute s (Invoke name as) = Invoke name (map (substitute s) as)
  substitute _ g = error "We have only planned to substitute into calls."
  --substitute _ g = error $ printf "We have only planned to substitute into calls, and you are trying to substitute into:\n%s" (show g)

substituteList :: (Ord v) => Subst v -> [G v] -> [G v]
substituteList = map . substitute

-- instance ApplySubst (forall v. [G v]) where
--   substitute = map . substitute

---- Composing substitutions
o :: (Ord v) => Subst v -> Subst v -> Subst v
o sigma theta =
  if null $ intersection sigma theta
  then
    Subst $ Map.fromList $ map (\ (s, ts) -> (s, substitute sigma ts)) (toList theta) ++ toList sigma
  else
    error "Non-disjoint domains in substitution composition"

instance (Dot v) => Dot (Subst v) where
  dot (Subst s) = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (dot $ V x) (dot y)) (Map.toList s)))

instance (Show v) => Show (Subst v) where
  show (Subst s) = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (show $ V x) (show y)) (Map.toList s)))

showSubst' :: (Show v) => Subst v -> String
showSubst' s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s -> %s" (show $ V x) (show y)) (Map.toList (getSubst s))))

