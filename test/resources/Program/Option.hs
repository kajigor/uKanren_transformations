module Program.Option where

import Syntax

none :: Term a
none = C "none" []

some :: Term a -> Term a
some x = C "some" [x]