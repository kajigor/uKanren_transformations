module Residualize where

import Syntax
--import Tree
import Driving
import qualified Eval as E  
import Data.List
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map

toX :: Term S -> Term X
toX (V x)    = V $ ("x"++) $ show x
toX (C c ts) = C c $ map toX ts

residualize :: (TreeContext, Tree) -> G X
residualize (tc, t) = residualizeGen [] tc t where
  residualizeGen g _ Fail         = Invoke "failure" []
  residualizeGen g _ (Success []) = Invoke "success" []
  residualizeGen g _ (Success s ) =  
    conj $ map (\ (s, ts) -> (toX {-$ E.substitute g $-} $ V s) :=: (toX $ E.substitute g ts)) $ reverse s
  residualizeGen g _          (Rename id _ r    ) = Invoke (fident id) [V $ vident $ snd x | x <- r] 
  residualizeGen g tc (Or     l r _     ) = residualizeGen g tc l ||| residualizeGen g tc r
  residualizeGen g tc (Split  id l r _  ) = scope tc id $ residualizeGen g tc l &&& residualizeGen g tc r
  residualizeGen g tc (Gen    id g' t _ ) = scope tc id $ residualizeGen (g' `E.o` g) tc t
  residualizeGen g tc (Call   id s    _ ) = scope tc id $ residualizeGen g tc s
  fident id = "f" ++ show id  
  vident id = "v" ++ show id
  scope (sr, args, _) id g =
    let fargs = map vident (args Map.! id) in
    if Set.member id sr
    then Let (def (fident id) fargs g) (Invoke (fident id) $ map V fargs)
    else g    