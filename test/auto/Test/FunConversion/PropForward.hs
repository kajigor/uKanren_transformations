{-# LANGUAGE TemplateHaskell #-}
module Test.FunConversion.PropForward where

import Test.FunConversion.PropProg (evalo)
import Program (Program(Program))

import qualified Mode.Toplevel as TL
import qualified Mode.Pretty as TP
import qualified Mode.NormSyntax as TN
-- $(return $ S.embedProg "eval" $ TR.transProg "evalo" [0, 2] (Program evalo undefined))

-- unit_PropForward = print $ takeS 5 $ eval (Cons (Trueo) Zero) Trueo

-- unit_PropForward = putStrLn $ TH.pprint (S.embedProg "evalo" $ TR.transProg "evalo" [0, 2] (Program evalo undefined))
unit_PropForward = case TL.topLevelWithDefaultCall (Program evalo undefined) "evalo" [0, 2] of 
    Left e -> error e
    Right m -> putStrLn $ TP.prettyString $ TN.back m