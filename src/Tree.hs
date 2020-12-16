module Tree where

import qualified Eval as E
import Syntax
import Debug.Trace
import Text.Printf
import Generalization

type Id = Int
data Tree =
  Prune   [G S]                                   |
  Fail                                            |
  Success E.MapSigma                              |
  Or      Tree Tree (G S) E.MapSigma              |
  Rename  Id (G S) E.MapSigma Renaming E.MapSigma |
  Gen     Id Generalizer Tree (G S) E.MapSigma    |
  Call    Id Tree (G S) E.MapSigma                |
  Split   Id [Tree] (G S) E.MapSigma              deriving Show

-- Renaming
type Renaming = [(S, S)]

instance Dot Tree where
  dot (Prune gs) =
    case conj gs of
      Nothing -> "Prune"
      Just goal -> printf "Prune <BR/> %s" (dot goal)
  dot Fail = "_|_"
  dot (Success s)           = printf "S" -- <BR/> %s" (E.showSigma s)
  dot (Rename id' g s ts _) = printf "R" -- %s <BR/> %s <BR/> %s <BR/> %s" (show id') (E.showSigma s) (dot g) (dot $ reverse ts)
  dot (Gen id' g _ curr _)  = printf "G"-- %s <BR/> %s <BR/> %s" (show id') (dot g) (dot curr)
  dot (Or _ _ curr _)       = printf "O" -- <BR/> " ++ dot curr
  dot (Split id' _ curr _)  = printf "Splt" -- %s <BR/> %s" (show id') (dot curr)
  dot (Call id' _ curr _)   = printf "Call %s <BR/> %s" (show id') (dot curr) -- ++ " <BR/> " ++ dot s
