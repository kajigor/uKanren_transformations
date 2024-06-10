module Program.Pair where

import           Syntax

pair :: Term a -> Term a -> Term a
pair x y = C "pair" [x, y]
