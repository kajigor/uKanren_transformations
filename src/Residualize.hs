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


toX :: Term S -> Term X
toX (V x)    = V $ ("x"++) $ show x
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
simpl (Split id l r g s) = 
  case simpl l of 
    Fail -> Fail 
    l'   -> case simpl r of 
              Fail -> Fail
              r'   -> Split id l' r' g s
simpl t = t 

--simplConj = (&&&)
simplConj (Invoke success []) g = g
simplConj g (Invoke success []) = g
simplConj g g1 = g &&& g1 

success = "success"
failure = "failure"

residualize :: (TreeContext, Tree) -> G X
residualize (tc, t) = E.post_eval' $ scope tc 0 $ residualizeGen [] tc (simpl t) [] where
  residualizeGen _ _ Fail         _ = Invoke failure [] 
  residualizeGen g _ (Success s ) ubst = 
    let delta = s \\ ubst in
    if delta == []
    then Invoke success []
    else residualizeSubst g delta
  residualizeGen g tc (Rename id _ s r s' )  ubst = substCon g s' ubst $ simplConj (residualizeGen g tc (Success s) s') (Invoke (fident id) (reverse [V $ vident $ snd x | x <- r]))
  residualizeGen g tc (Or     l r _    s' )  ubst = substCon g s' ubst $ residualizeGen g tc l s' ||| residualizeGen g tc r s'
  residualizeGen g tc (Split  id l r _ s' )  ubst = substCon g s' ubst $ scope tc id $ simplConj (residualizeGen g tc l s') (residualizeGen g tc r s')
  residualizeGen g tc (Gen    id g' t _ s' ) ubst = substCon g s' ubst $ scope tc id $ residualizeGen (g' `E.o` g) tc t s'
  residualizeGen g tc (Call   id s    _ s' ) ubst = substCon g s' ubst $ scope tc id $ residualizeGen g tc s s'
  fident id = "f" ++ show id  
  vident id = "x" ++ show id 
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

redtest   = let r = residualize tc in trace ("\n\n" ++ show r ++ "\n\n") r
redtest'  = let r = residualize tc' 
                ((x, y, _), _) = tc' 
            in trace ("\n\n" ++ show r ++ "\n\n" ++ show x ++ "\n\n" ++ show y) r
redtest'' = let r = residualize tc'' in trace ("\n\n" ++ show r ++ "\n\n") r



























{- module Residualize where

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


toX :: Term S -> Term X
toX (V x)    = V $ ("x"++) $ show x
toX (C c ts) = C c $ map toX ts

project s vars = filter (\ (x, _) -> isJust $ find (== x) vars) s 

residualize :: (TreeContext, Tree) -> G X
residualize (tc, t) = residualizeGen [] tc t [] where
  residualizeGen _ _ Fail         _    = Invoke "failure" []
  residualizeGen _ _ (Success []) _    = Invoke "success" []
  residualizeGen g _ (Success s ) vars =  
    conj $ map (\ (s, ts) -> (toX {-$ E.substitute g $-} $ V s) :=: (toX $ E.substitute g ts)) $ reverse $ project s vars
--  residualizeGen g _          (Rename id _ r    ) = Invoke (fident id) [V $ vident $ snd x | x <- r] 
  residualizeGen g tc (Rename id _ s r  ) vars = residualizeGen g tc (Success s) vars &&& Invoke (fident id) [V $ vident $ snd x | x <- r] 
  residualizeGen _ _  (Or Fail Fail _ )   _    = Invoke "failure" []
  residualizeGen g tc (Or Fail  r _     ) vars = residualizeGen g tc r vars
  residualizeGen g tc (Or l Fail  _     ) vars = residualizeGen g tc l vars
  residualizeGen g tc (Or     l r _     ) vars = residualizeGen g tc l vars ||| residualizeGen g tc r vars
  residualizeGen g tc (Split  id l r _  ) vars = scope tc id vars $ (\vs -> residualizeGen g tc l vs &&& residualizeGen g tc r vs)
  residualizeGen g tc (Gen    id g' t _ ) vars = scope tc id vars $ residualizeGen (g' `E.o` g) tc t
  residualizeGen g tc (Call   id s    _ ) vars = scope tc id vars $ residualizeGen g tc s
  fident id = "f" ++ show id  
  vident id = "v" ++ show id
  scope tc@(sr, args, _) id vars toG =
    if Set.member id sr
    then let as = args Map.! id in 
         let fargs = map vident as in 
         Let (def (fident id) fargs (toG as)) (Invoke (fident id) $ map V fargs)
    else toG vars

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

redtest = residualize tc
redtest' = residualize tc' -}
