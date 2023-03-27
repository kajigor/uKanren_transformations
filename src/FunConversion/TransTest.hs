{-# LANGUAGE TemplateHaskell #-}
module FunConversion.TransTest where
import FunConversion.Trans (transProg, addoDef, muloDef)
import Program
import FunConversion.Syntax (embedProg)
import Control.Monad (msum, guard)
import Stream

import qualified FunConversion.Syntax as F
import qualified Mode.NormSyntax as M
import qualified Mode.Term as M
import qualified Data.List.NonEmpty as NE
import qualified Syntax as S
import Def

import qualified Language.Haskell.TH as TH

$(return $ embedProg "mulo" $ transProg "mulo" [1, 2] (Program [addoDef, muloDef] (error "accesed original goal")))


testTrans :: IO ()
testTrans = print $ takeS 1 $ mulo (S (S O)) (S (S (S (S O))))
-- testTrans = case F.toQuote <$> transProg "mulo" [1, 2] (Program [addoDef, muloDef] (error "accesed original goal")) :: Either String (Either F.Error F.ProgramDec) of
--   Left e -> print e
--   Right (Left e) -> print e
--   Right (Right (F.ProgramDec decs _)) -> do
--     putStrLn (TH.pprint decs)