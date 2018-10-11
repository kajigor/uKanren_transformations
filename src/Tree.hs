module Tree where

import qualified Eval as E
import Syntax
import Debug.Trace
import Text.Printf

type Id = Int
data Tree =
  Prune   [G S]                             |
  Fail                                      |
  Success E.Sigma                           |
  Or      Tree Tree (G S) E.Sigma           |
  Rename  Id (G S) E.Sigma Renaming E.Sigma |
  Gen     Id Generalizer Tree (G S) E.Sigma |
  Call    Id Tree (G S) E.Sigma             |
  Split   Id [Tree] (G S) E.Sigma           deriving Show

-- Renaming
type Renaming = [(S, S)]

---- Generalization
type Generalizer = E.Sigma


-- TODO remove
conj :: [G a] -> G a
conj [] = error "Empty conjunction"
conj (a:as) = foldl (:/\:) a as


instance Dot Tree where
  dot (Prune gs) = trace (case gs of [x] -> show x ; _ -> "") $ printf "Prune <BR/> %s" (dot $ conj gs)
  dot Fail = "_|_"
  dot (Success s)           = printf "S" -- <BR/> %s" (E.showSigma s)
  dot (Rename id' g s ts _) = printf "R" -- %s <BR/> %s <BR/> %s <BR/> %s" (show id') (E.showSigma s) (dot g) (dot $ reverse ts)
  dot (Gen id' g _ curr _)  = printf "G"-- %s <BR/> %s <BR/> %s" (show id') (dot g) (dot curr)
  dot (Or _ _ curr _)       = printf "O" -- <BR/> " ++ dot curr
  dot (Split id' _ curr _)  = printf "Splt" -- %s <BR/> %s" (show id') (dot curr)
  dot (Call id' _ curr _)   = printf "Call %s <BR/> %s" (show id') (dot curr) -- ++ " <BR/> " ++ dot s
