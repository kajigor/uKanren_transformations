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
