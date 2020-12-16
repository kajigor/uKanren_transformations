{-# LANGUAGE FlexibleInstances #-}

module Subst where

import Syntax
import qualified Data.Map.Strict as Map
import Text.Printf
import Data.List ( intercalate )
import Prelude hiding ( lookup )

type Subst = Map.Map S Ts

empty :: Subst
empty = Map.empty

lookup :: S -> Subst -> Maybe Ts
lookup = Map.lookup

insert :: S -> Ts -> Subst -> Subst
insert = Map.insert

length :: Subst -> Int
length = Map.size

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
  if null $ Map.intersection sigma theta
  then
    Map.fromList $ map (\ (s, ts) -> (s, substitute sigma ts)) (Map.toList theta) ++ (Map.toList sigma)
  else
    error "Non-disjoint domains in substitution composition"

dotSubst :: Subst -> String
dotSubst s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (dot $ V x) (dot y)) (Map.toList s)))

instance Dot Subst where
  dot = dotSubst

showSubst :: Subst -> String
showSubst s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s &rarr; %s" (show $ V x) (show y)) (Map.toList s)))

showSubst' :: Subst -> String
showSubst' s = printf " [ %s ] " (intercalate ", " (map (\(x,y) -> printf "%s -> %s" (show $ V x) (show y)) (Map.toList s)))


