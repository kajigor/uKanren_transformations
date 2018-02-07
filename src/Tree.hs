module Tree where 

import qualified Eval as E
import Syntax

type Id = Int
data Tree = 
  Fail                                      |
  Success E.Sigma                           |
  Or      Tree Tree (G S) E.Sigma           |
  Rename  Id (G S) E.Sigma Renaming E.Sigma |
  Gen     Id Generalizer Tree (G S) E.Sigma |
  Call    Id Tree (G S) E.Sigma             |
  Split   Id Tree Tree (G S) E.Sigma deriving Show

-- Renaming
type Renaming = [(S, S)]

---- Generalization
type Generalizer = E.Sigma

instance Dot Tree where
  dot Fail = "_|_"
  dot (Success s)           = "S <BR/> " ++ E.showSigma s
  dot (Rename id g s ts _)    = "R " ++ show id ++ " <BR/> " ++ E.showSigma s ++ " <BR/> " ++ dot g ++ " <BR/> " ++ dot (reverse ts)
  dot (Gen id g _ curr _)     = "G " ++ show id ++ " <BR/> " ++ dot g ++ " <BR/> " ++ dot curr
  dot (Or _ _ curr _)         = "O" -- <BR/> " ++ dot curr
  dot (Split id t1 t2 curr _) = "Splt " ++ show id ++ " <BR/> " ++ dot curr
  dot (Call id t curr s)      = "Call " ++ show id ++ " <BR/> " ++ dot curr -- ++ " <BR/> " ++ dot s 
