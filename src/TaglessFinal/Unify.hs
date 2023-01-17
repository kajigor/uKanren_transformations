module TaglessFinal.Unify where

import qualified TaglessFinal.Subst as Subst
import           TaglessFinal.Term

unifyG :: Ord a
       => (Subst.Subst a -> a -> Term a -> Bool)
       -> Maybe (Subst.Subst a)
       -> Term a
       -> Term a
       -> Maybe (Subst.Subst a)
unifyG _ Nothing _ _ = Nothing
unifyG f st@(Just subst) u v =
    unify' (walk subst u) (walk subst v)
  where
    unify' (Var u') (Var v') | u' == v' = Just subst
    unify' (Var u') (Var v') = Just $ Subst.insert (min u' v') (Var $ max u' v') subst
    unify' (Var u') t =
      if f subst u' t
      then Nothing
      else return $ Subst.insert u' v subst
    unify' t (Var v') =
      if f subst v' t
      then Nothing
      else return $ Subst.insert v' u subst
    unify' (Con a as) (Con b bs) | a == b && length as == length bs =
      foldl (\ st' (u', v') -> unifyG f st' u' v') st $ zip as bs
    unify' _ _ = Nothing

walk :: Ord a => Subst.Subst a -> Term a -> Term a
walk s x@(Var v) =
  maybe x (walk s) $ Subst.lookup v s
walk _ u = u

-- Unification
unify :: Ord a => Maybe (Subst.Subst a) -> Term a -> Term a -> Maybe (Subst.Subst a)
unify =
    unifyG occursCheck
  where
    occursCheck :: Ord a => Subst.Subst a -> a -> Term a -> Bool
    occursCheck s u t =
      case walk s t of
        Var v -> v == u
        Con _ as -> any (occursCheck s u) as
