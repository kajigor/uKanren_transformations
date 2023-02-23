{-# LANGUAGE TemplateHaskell #-}
module Test.FunConversion.NumForward where

import Program.Num
import Program (Program(Program))
import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S

import Control.Monad (msum, guard)
import Stream

$(return $ S.embedProg $ TR.transProg "mulo" [0, 1] (Program mulo undefined))
