module Test.Mode.NormSyntax where

import Program.Num
import qualified Syntax as S
import Mode.NormSyntax
import Mode.Syntax (flatten)
import Mode.Term
import Program
import Def
import VarRename
import Test.Helper ((@?=))
import Data.List.NonEmpty

x = FTVar $ Var "x"
y = FTVar $ Var "y"
c = Call "c" [Var "x", Var "y"]
d = Call "d" [Var "x"]
e = Call "e" [Var "y"]

runTest program expected =
  case uniquelyRenameVars program of
    Just (program, nextVar) -> do
      let flat = normalize $ flatten program nextVar
      flat @?= expected
    Nothing -> fail "Unique rename failed"


unit_normSyntax :: IO ()
unit_normSyntax = do
  runTest
    (Program [] (S.fresh ["x", "y", "z"] $ S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.call "b" [S.V "y"]]]))
    (Program {getDefs = [Def {getName = "topLevel0", getArgs = [2,1], getBody = Disj (Conj (Call "a" [Var {getVar = 2}] :| []) :| [Conj (Call "b" [Var {getVar = 1}] :| [])])}], getGoal = Disj (Conj (Call "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call "topLevel0" [Var {getVar = 2},Var {getVar = 1}]]) :| [])})

  runTest
    (Program [] (S.fresh ["x", "y", "z"] $ S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.call "b" [S.V "y"]]]]]))
    Program {getDefs = [Def {getName = "topLevel0", getArgs = [2,0,1], getBody = Disj (Conj (Call "a" [Var {getVar = 2}] :| []) :| [Conj (Call "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call "topLevel00" [Var {getVar = 2},Var {getVar = 1}]])])},Def {getName = "topLevel00", getArgs = [2,1], getBody = Disj (Conj (Call "a" [Var {getVar = 2}] :| []) :| [Conj (Call "b" [Var {getVar = 1}] :| [])])}], getGoal = Disj (Conj (Call "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call "topLevel0" [Var {getVar = 2},Var {getVar = 0},Var {getVar = 1}]]) :| [])}


