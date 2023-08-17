module Residualization where

import           Driving
import qualified Subst
import           Syntax
import           Tree


toX :: Term S -> Term X
toX = (vident <$>)

residualizeSubst :: Subst.Subst S -> Subst.Subst S -> G X
residualizeSubst g s = unsafeConj $ map (\ (s', ts) -> toX (V s') :=: toX (Subst.substitute g ts)) $ reverse (Subst.toList s)

substCon :: Subst.Subst S -> Subst.Subst S -> Subst.Subst S -> G X -> G X
substCon gen s ubst g =
  let delta = Subst.difference s ubst in
  if Subst.null delta
  then g
  else residualizeSubst gen delta &&& g

simpl :: Tree -> Tree
simpl (Or l r g s) =
  case simpl l of
    Fail -> simpl r
    l'   -> case simpl r of
              Fail -> l'
              r'   -> Or l' r' g s
simpl (Gen id' gen ch g s) =
  case simpl ch of
    Fail -> Fail
    ch'  -> Gen id' gen ch' g s
simpl (Call id' ch g s) =
  case simpl ch of
    Fail -> Fail
    ch'  -> Call id' ch' g s
simpl (Split id' ts g s) =
  let simplified = map simpl ts
  in  if any (\x -> case x of Fail -> True; _ -> False) simplified then Fail else Split id' simplified g s
simpl t = t

simplConj :: [G a] -> G a
simplConj conj' =
  let noSuccess = filter (not . isSuccess) conj'
  in  if   not $ null noSuccess
      then unsafeConj noSuccess
      else success

vident = ('x' :) . show

residualize :: (TreeContext, Tree, [Id]) -> (G X, [String])
residualize (tc, t, args) = undefined
  -- (E.postEval (vident <$> args) $ residualizeGen [] tc (simpl t) [], vident <$> args)
  -- where
  --   residualizeGen _ _ Fail         _ = failure
  --   residualizeGen g _ (Success s ) ubst =
  --     let delta = s \\ ubst in
  --     if null delta
  --     then success
  --     else residualizeSubst g delta
  --   residualizeGen g tc (Rename id _ s r s' )  ubst = substCon g s' ubst $ simplConj [residualizeGen g tc (Success s) s', Invoke (fident id) (reverse [V $ vident $ snd x | x <- r])]
  --   residualizeGen g tc (Or     l r _    s' )  ubst = substCon g s' ubst $ residualizeGen g tc l s' ||| residualizeGen g tc r s'
  --   residualizeGen g tc (Split  id ts _ s' )   ubst = substCon g s' ubst $ scope tc id $ simplConj $ map (\x -> residualizeGen g tc x s') ts
  --   residualizeGen g tc (Gen    id g' t _ s' ) ubst = substCon g (s' ++ g') ubst $ scope tc id $ residualizeGen (g' `E.o` g) tc t s'
  --   residualizeGen g tc (Call   id s    _ s' ) ubst = substCon g s' ubst $ scope tc id $ residualizeGen g tc s s'
  --   residualizeGen _ _ _ _ = error "Oops, something is wrong in residualizeGen"
  --   fident id' = 'f' : show id'
  --   scope (sr, args, _) id g =
  --     if Set.member id sr
  --     then let as = reverse $ args Map.! id in
  --          let fargs = vident <$> as in
  --          Let (Def (fident id) fargs g) (Invoke (fident id) $ map V fargs)
  --     else g
