module Residualize where

import Syntax
import Tree
import Driving
import qualified Eval as E
import Data.List
import Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map
import Debug.Trace
import List hiding (c)
import Text.Printf


toX :: Term S -> Term X
toX (V x)    = V ('x' : show x)
toX (C c ts) = C c $ map toX ts

residualizeSubst :: E.Sigma -> [(S, Ts)] -> G X
residualizeSubst g s = conj $ map (\ (s', ts) -> toX (V s') :=: toX (E.substitute g ts)) $ reverse s

substCon :: E.Sigma -> [(S, Ts)] -> [(S, Ts)] -> G X -> G X
substCon gen s ubst g =
  let delta = s \\ ubst in
  if null delta
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
  let noSuccess = filter (\x -> case x of (Invoke "success" []) -> False; _ -> True) conj'
  in  if   not $ null noSuccess
      then foldl1 (&&&) noSuccess
      else Invoke success []

success :: String
success = "success"

failure :: String
failure = "failure"

residualize :: (TreeContext, Tree, [Id]) -> (G X, [String])
residualize (tc, t, args) =
  (E.postEval' (map vident args) $ residualizeGen [] tc (simpl t) [], map vident args)
  where
    residualizeGen _ _ Fail         _ = Invoke failure []
    residualizeGen g _ (Success s ) ubst =
      let delta = s \\ ubst in
      if null delta
      then Invoke success []
      else residualizeSubst g delta
    residualizeGen g tc (Rename id _ s r s' )  ubst = substCon g s' ubst $ simplConj [residualizeGen g tc (Success s) s', Invoke (fident id) (reverse [V $ vident $ snd x | x <- r])]
    residualizeGen g tc (Or     l r _    s' )  ubst = substCon g s' ubst $ residualizeGen g tc l s' ||| residualizeGen g tc r s'
    residualizeGen g tc (Split  id ts _ s' )   ubst = substCon g s' ubst $ scope tc id $ simplConj $ map (\x -> residualizeGen g tc x s') ts
    residualizeGen g tc (Gen    id g' t _ s' ) ubst = substCon g (s' ++ g') ubst $ scope tc id $ residualizeGen (g' `E.o` g) tc t s'
    residualizeGen g tc (Call   id s    _ s' ) ubst = substCon g s' ubst $ scope tc id $ residualizeGen g tc s s'
    residualizeGen _ _ _ _ = error "Oops, something is wrong in residualizeGen"
    fident id' = 'f' : show id'
    vident id' = 'x' : show id'
    scope (sr, args, _) id g =
      if Set.member id sr
      then let as = reverse $ args Map.! id in
           let fargs = map vident as in
           Let (def (fident id) fargs g) (Invoke (fident id) $ map V fargs)
      else g


-- Purification of non-essential variables
purification :: (G X, [String]) -> (G X, [String])
purification (goal, args) = (fresh finalVars purifiedGoal, args) where
  getEssentialVars (g1 :/\: g2)        s = getEssentialVars g2 $ getEssentialVars g1 s
  getEssentialVars (g1 :\/: g2)        s = getEssentialVars g2 $ getEssentialVars g1 s
  getEssentialVars (Let (_, _, g1) g2) s = getEssentialVars g2 $ getEssentialVars g1 s
  getEssentialVars (t1 :=:  t2)        s = let vars = Set.fromList $ fv t1 ++ fv t2 in
                                           if Set.null $ Set.intersection s vars then s else Set.union s vars
  getEssentialVars (Invoke _ args)     s = let vars = Set.fromList $ concatMap fv args in Set.union s vars
  getEssentialVars g                   s = getEssentialVars (snd $ freshVars [] g) s

  getAllEssentialVars g s = let s' = getEssentialVars g s in if s == s' then s else getAllEssentialVars g s'

  purificationGoal s g@(t1 :=:  t2) = if Set.null $ Set.intersection s $ Set.fromList $ fv t1 ++ fv t2 then Nothing else Just g
  purificationGoal s   (g1 :\/: g2) = Just $ fromMaybe (call success []) (purificationGoal s g1) :\/: fromMaybe (call success []) (purificationGoal s g2)
  purificationGoal s   (g1 :/\: g2) =
    case (purificationGoal s g1, purificationGoal s g2) of
      (Nothing, Nothing) -> Nothing
      (Just g,  Nothing) -> Just g
      (Nothing, Just g ) -> Just g
      (Just g1, Just g2) -> Just $ g1 :/\: g2
  purificationGoal s   g            = Just g

  (vars, unfreshGoal)   = freshVars [] goal

  essentialVars = getAllEssentialVars unfreshGoal $ Set.fromList args
  
  purifiedGoal = 
    case purificationGoal essentialVars unfreshGoal of
      Just g  -> g
      Nothing -> call success []
  
  finalVars = (Set.toList $ Set.intersection essentialVars $ Set.fromList vars) \\ args
