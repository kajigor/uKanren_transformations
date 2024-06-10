module Util.String where

import qualified Data.Char as C

modifyFirstLetter :: (Char -> Char) -> String -> String
modifyFirstLetter _ "" = ""
modifyFirstLetter f x = f (head x) : tail x

toLower :: String -> String
toLower = modifyFirstLetter C.toLower

toUpper :: String -> String
toUpper = modifyFirstLetter C.toUpper
