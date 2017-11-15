module Residualize where

import Syntax
--import Tree
import Driving
import qualified Eval as E  
import Data.List
import qualified Data.Set as Set

toX :: Term S -> Term X
toX (V x)    = V $ ("x"++) $ show x
toX (C c ts) = C c $ map toX ts

residualize :: (TreeContext, Tree) -> G X
residualize (tc, t) = residualizeGen [] tc t where
  residualizeGen g _ Fail         = Invoke "failure" []
  residualizeGen g _ (Success []) = Invoke "success" []
  residualizeGen g _ (Success s ) =  
    conj $ map (\ (s, ts) -> (toX $ E.substitute g $ V s) :=: (toX $ E.substitute g ts)) $ reverse s
  residualizeGen g tc@(sr, _) (Rename id _ _    ) = Invoke (show id) [] 
  residualizeGen g tc         (Or     l r _     ) = residualizeGen g tc l ||| residualizeGen g tc r

  residualizeGen g tc@(sr, _) (Split  id l r _  ) = scope sr id $ residualizeGen g tc l &&& residualizeGen g tc r
  residualizeGen g tc@(sr, _) (Gen    id g' t _ ) = scope sr id $ residualizeGen (g' `E.o` g) tc t
  residualizeGen g tc@(sr, _) (Call   id s    _ ) = scope sr id $ residualizeGen g tc s 
  
  scope sr id g =
    if Set.member id sr
    then Let (def (show id) [] g) (Invoke (show id) [])
    else g    