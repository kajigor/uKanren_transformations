module Test.OCanrenize where

import           OCanrenize
import           Syntax
import           Test.Helper (test)

unit_OCanrenizeTerm = do
    test ocanren (C "fst" [] :: Term X) "(fst_ ())"
    test ocanren (C "snd" [] :: Term X) "(snd_ ())"
    test ocanren (C "fill" [] :: Term X) "(fill ())"
