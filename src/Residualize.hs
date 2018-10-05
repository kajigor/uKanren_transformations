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
import List
import Text.Printf


toX :: Term S -> Term X
toX (V x)    = V $ ('x' : show x)
toX (C c ts) = C c $ map toX ts

residualizeSubst g s = conj $ map (\ (s, ts) -> (toX $ V s) :=: (toX $ E.substitute g ts)) $ reverse s

substCon gen s ubst g =
  let delta = s \\ ubst in
  if delta == []
  then g
  else residualizeSubst gen delta &&& g

simpl (Or l r g s) =
  case simpl l of
    Fail -> simpl r
    l'   -> case simpl r of
              Fail -> l'
              r'   -> Or l' r' g s
simpl (Gen id gen ch g s) =
  case simpl ch of
    Fail -> Fail
    ch'  -> Gen id gen ch' g s
simpl (Call id ch g s) =
  case simpl ch of
    Fail -> Fail
    ch'  -> Call id ch' g s
simpl (Split id ts g s) =
  let simplified = map simpl ts
  in  if any (\x -> case x of Fail -> True; _ -> False) simplified then Fail else Split id simplified g s
simpl t = t

simplConj conj =
  let noSuccess = filter (\x -> case x of (Invoke success []) -> False; _ -> True) conj
  in  if   length noSuccess > 0
      then foldl1 (&&&) noSuccess
      else (Invoke success [])

success = "success"
failure = "failure"

residualize :: (TreeContext, Tree, [Id]) -> (G X, [String])
residualize (tc, t, args) =
  (E.post_eval' (map vident args) $ residualizeGen [] tc (simpl t) [], map vident args)
  where
    residualizeGen _ _ Fail         _ = Invoke failure []
    residualizeGen g _ (Success s ) ubst =
      let delta = s \\ ubst in
      if delta == []
      then Invoke success []
      else residualizeSubst g delta
    residualizeGen g tc (Rename id _ s r s' )  ubst = substCon g s' ubst $ simplConj [residualizeGen g tc (Success s) s', Invoke (fident id) (reverse [V $ vident $ snd x | x <- r])]
    residualizeGen g tc (Or     l r _    s' )  ubst = substCon g s' ubst $ residualizeGen g tc l s' ||| residualizeGen g tc r s'
    residualizeGen g tc (Split  id ts _ s' )   ubst = substCon g s' ubst $ scope tc id $ simplConj $ map (\x -> residualizeGen g tc x s') ts
    residualizeGen g tc (Gen    id g' t _ s' ) ubst = substCon g (s' ++ g') ubst $ scope tc id $ residualizeGen (g' `E.o` g) tc t s'
    residualizeGen g tc (Call   id s    _ s' ) ubst = substCon g s' ubst $ scope tc id $ residualizeGen g tc s s'
    fident id = 'f' : show id
    vident id = 'x' : show id
    scope tc@(sr, args, _) id g =
      if Set.member id sr
      then let as = reverse $ args Map.! id in
           let fargs = map vident as in
           Let (def (fident id) fargs g) (Invoke (fident id) $ map V fargs)
      else g

scoping =
  let f g =
        let x = V "x"
            y = V "y"
            z = V "z"
            t = V "t"
        in Let (def "f" ["x", "y", "t"]
                 (fresh ["z"] ( z === x % y &&& t === z % nil))
               ) g
  in residualize (drive (f ( fresh ["x","y","t"] $ call "f" [V "x", V "y", V "t"])))

appendo g =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  Let
    (def "appendo" ["x", "y", "xy"]
         ((x === nil &&& xy === y) |||
          (fresh ["h", "t", "ty"]
             (x  === h % t  &&&
              xy === h % ty &&&
              call "appendo" [t, y, ty]
             )
          )
         )
    ) g

redtest   = let (r, _) = residualize tc in trace (printf "\n\n%s\n\n" $ show r) r
redtest'  = let (r, _) = residualize tc'
                ((x, y, _), _, _) = tc'
            in trace (printf "\n\n%s\n\n%s\n\n%s" (show r) (show x) (show y)) r
redtest'' = let (r, _) = residualize tc'' in trace (printf "\n\n%s\n\n" $ show r) r


