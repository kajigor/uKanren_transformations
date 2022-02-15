module TaglessFinal.Term where


type Name = String
type Var = String

data Term a = Var a | Con Name [Term a]
            deriving (Show, Eq)

