module TaglessFinal.Term where

import qualified Syntax
import           Text.Printf (printf)

type Name = String
type Var = String

data Term a = Var a | Con Name [Term a]
            deriving (Show, Eq)

toFreshVar :: Int -> Var
toFreshVar = printf "x.%s" . show

toVar :: Int -> Var
toVar = printf "v.%s" . show

toSyntax :: Term a -> Syntax.Term a
toSyntax (Var v) = Syntax.V v
toSyntax (Con n args) = Syntax.C n $ map toSyntax args
