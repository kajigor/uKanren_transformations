{-
   Terms with named variables
-}

module Term where

import Data.List
import Data.Maybe

class Term t where
  var     :: t -> Maybe String
  binds   :: t -> Maybe String
  sub     :: t -> [t]
  make    :: t -> [t] -> t
  makeVar :: String -> t
  rename  :: String -> t -> t
  eq      :: t -> t -> Bool

hom :: Term t => (t -> t) -> t -> t
hom f t = (\ t' -> make t' $ map (hom f) (sub t')) (f t)

fold :: Term t => (a -> t -> a) -> a -> t -> a
fold f init t = f (foldl (fold f) init (sub t)) t

fv :: Term t => t -> [String]
fv t = fold (\ vs t -> (vs \\ (maybeToList $ binds t)) `union` (maybeToList $ var t)) [] t

subst :: Term t => String -> t -> t -> t
subst x s t = 
  case var t of
    Just y  -> if x == y then s else t
    Nothing -> case binds t of
                 Just y  -> if x == y 
                            then t 
                            else if elem y vs 
                                 then let y' = head $ names \\ (vs ++ fv t) in
                                      subst x s $ rename y' $ make t (map (subst y $ makeVar y') $ sub t)
                                 else recurse
                 Nothing -> recurse
  where
    recurse = make t (map (subst x s) $ sub t) 
    vs      = fv s    
    names   = letters ++ [reverse $ a ++ n | n <- names, a <- letters] where
      letters = [[x] | x <- ['a'..'z']]

alpha :: Term t => t -> t -> Maybe [(String, String)]
alpha = alpha' (Just []) where
  alpha' f m n =
    if eq m n 
    then case (binds m, binds n) of
           (Just mb, Just nb) -> recurse $ extend f mb nb
           _                  -> case (var m, var n) of
                                   (Just mv, Just nv) -> extend f mv nv
                                   _                  -> recurse f
    else Nothing
    where
      recurse f    = foldl (\ f (m', n') -> alpha' f m' n') f (zip (sub m) (sub n))
      extend f x y = f >>= (\ f -> case lookup x f of
                                     Nothing -> if notElem y (map snd f) then Just ((x, y) : f) else Nothing
                                     Just x' -> if x' == y then Just f else Nothing
                           )
