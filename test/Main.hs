-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  
-- License     :  AllRightsReserved
--
-- Maintainer  :  
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Main.hs"]


