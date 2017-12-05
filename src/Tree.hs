module Tree where 

import qualified Eval as E
import Syntax

type Id = Int
data Tree = 
  Fail                              | 
  Success E.Sigma                   | 
  Or      Tree Tree (G S)           | 
  Rename  Id (G S) Renaming         |
  Gen     Id Generalizer Tree (G S) | 
  Call    Id Tree (G S)             |
  Split   Id Tree Tree (G S) deriving Show

-- Renaming
type Renaming = [(S, S)]

---- Generalization
type Generalizer = E.Sigma
