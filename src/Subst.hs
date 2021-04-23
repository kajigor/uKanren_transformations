{-# LANGUAGE FlexibleInstances #-}

module Subst where

import Syntax
import qualified Data.Map.Strict as Map
import Text.Printf ( printf )
import Data.List ( intercalate )
import Prelude hiding ( lookup, null )

newtype Subst = Subst { getSubst :: Map.Map S Ts }
              deriving Eq

empty :: Subst
empty = Subst Map.empty

lookup :: S -> Subst -> Maybe Ts
lookup k = Map.lookup k . getSubst

insert :: S -> Ts -> Subst -> Subst
insert k v (Subst s) = Subst $ Map.insert k v s

size :: Subst -> Int
size = Map.size . getSubst

difference :: Subst -> Subst -> Subst
difference (Subst x) (Subst y) = Subst $ Map.difference x y

intersection :: Subst -> Subst -> Subst
intersection (Subst x) (Subst y) = Subst $ Map.intersection x y

intersectionWith :: (Ts -> Ts -> b) -> Subst -> Subst -> Map.Map S b
intersectionWith f (Subst x) (Subst y) = Map.intersectionWith f x y

toList :: Subst -> [(S, Ts)]
toList = Map.toList . getSubst

null :: Subst -> Bool
null = Map.null . getSubst

fromList :: [(S, Ts)] -> Subst
fromList = Subst . Map.fromList

-- This is wacky: it may clash vars
union :: Subst -> Subst -> Subst
union (Subst s) (Subst s') = Subst $ Map.union s s'

-- Applying substitution
class ApplySubst a where
  substitute :: Subst -> a -> a

instance ApplySubst (Term S) where
  substitute s t@(V x) =
    case lookup x s of
      Just tx | tx /= t -> substitute s tx
      _                 -> t
  substitute s (C m ts) = C m $ map (substitute s) ts

instance ApplySubst (G S) where
  substitute s (Invoke name as) = Invoke name (map (substitute s) as)
  substitute _ g = error $ printf "We have only planned to substitute into calls, and you are trying to substitute into:\n%s" (show g)

instance ApplySubst [G S] where
  substitute = map . substitute

---- Composing substitutions
o :: Subst -> Subst -> Subst
o sigma theta =
  if null $ intersection sigma theta
  then
    Subst $ Map.fromList $ map (\ (s, ts) -> (s, substitute sigma ts)) (toList theta) ++ toList sigma
  else
    error "Non-disjoint domains in substitution composition"

instance Dot Subst where
  dot (Subst s) = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (dot $ V x) (dot y)) (Map.toList s)))

instance Show Subst where
  show (Subst s) = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (show $ V x) (show y)) (Map.toList s)))

showSubst' :: Subst -> String
showSubst' s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s -> %s" (show $ V x) (show y)) (Map.toList (getSubst s))))


