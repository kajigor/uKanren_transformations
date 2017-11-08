module Residualize where

import Syntax
--import Tree
import Driving
import qualified Eval as E  
import Data.List

toX :: Term S -> Term X
toX (V x)    = V $ ("x"++) $ show x
toX (C c ts) = C c $ map toX ts

residualize :: Tree -> G X
residualize = residualizeGen [] where
  residualizeGen g Fail         = Invoke "failure" []
  residualizeGen g (Success []) = Invoke "success" []
  residualizeGen g (Success s ) =  
    conj $ map (\ (s, ts) -> (toX $ E.substitute g $ V s) :=: (toX $ E.substitute g ts)) $ reverse s
  residualizeGen g (Or     l r _  ) = residualizeGen g l ||| residualizeGen g r
  residualizeGen g (Split  l r _  ) = residualizeGen g l &&& residualizeGen g r
  residualizeGen g (Rename _ _    ) = undefined
  residualizeGen g (Gen    g' t _ ) = residualizeGen (g' `E.o` g) t