module TaglessFinal.Term where

import Text.Printf ( printf )

type Name = String
type Var = String

data Term a = Var a | Con Name [Term a]
            deriving (Show, Eq)

toFreshVar :: Int -> Var
toFreshVar = printf "x.%s" . show

toVar :: Int -> Var
toVar = printf "v.%s" . show