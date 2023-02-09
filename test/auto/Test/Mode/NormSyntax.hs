module Test.Mode.NormSyntax where

import           Data.List.NonEmpty
import           Def
import           Mode.NormSyntax
import           Mode.Syntax        (flatten)
import           Mode.Term
import           Program
import           Program.Num
import qualified Syntax             as S
import           Test.Helper        ((@?=))
import           VarRename

x = FTVar $ Var "x"
y = FTVar $ Var "y"
c = Call NotDelayed "c" [Var "x", Var "y"]
d = Call NotDelayed "d" [Var "x"]
e = Call NotDelayed "e" [Var "y"]

runTest program expected =
  case uniquelyRenameVars program of
    Right (program, nextVar) -> do
      let flat = normalize $ flatten program nextVar
      flat @?= expected
    Left err -> fail $ "Unique rename failed\n" ++ err

unit_addo = do
  runTest
    (Program addo (S.fresh ["x", "y", "z"] $ S.call "addo" [S.V "x", S.V "y", S.V "z"]))
    (Program {getDefs = [Def {getName = "addo", getArgs = [0, 1, 2], getBody = Disj (Conj (Unif (Var { getVar = 0 } ) (FTCon "O" []) :| [Unif (Var {getVar = 2}) (FTVar (Var {getVar = 1}))]) :| [Conj (Unif (Var { getVar = 0 } ) (FTCon "S" [ Var {getVar = 3} ]) :| [Unif (Var { getVar = 2 } ) (FTCon "S" [ Var {getVar = 4} ]), Call NotDelayed "addo" [Var {getVar = 3},Var {getVar = 1},Var {getVar = 4}] ])])}], getGoal = Disj (Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| []) :| [])})

unit_normSyntax :: IO ()
unit_normSyntax = do
  runTest
    (Program [] (S.fresh ["x", "y", "z"] $ S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.call "b" [S.V "y"]]]))
    (Program {getDefs = [Def {getName = "topLevel0", getArgs = [2,1], getBody = Disj (Conj (Call NotDelayed "a" [Var {getVar = 2}] :| []) :| [Conj (Call NotDelayed "b" [Var {getVar = 1}] :| [])])}], getGoal = Disj (Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call NotDelayed "topLevel0" [Var {getVar = 2},Var {getVar = 1}]]) :| [])})

  runTest
    (Program [] (S.fresh ["x", "y", "z"] $ S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.call "b" [S.V "y"]]]]]))
    Program {getDefs = [Def {getName = "topLevel0", getArgs = [2,0,1], getBody = Disj (Conj (Call NotDelayed "a" [Var {getVar = 2}] :| []) :| [Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call NotDelayed "topLevel00" [Var {getVar = 2},Var {getVar = 1}]])])},Def {getName = "topLevel00", getArgs = [2,1], getBody = Disj (Conj (Call NotDelayed "a" [Var {getVar = 2}] :| []) :| [Conj (Call NotDelayed "b" [Var {getVar = 1}] :| [])])}], getGoal = Disj (Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call NotDelayed "topLevel0" [Var {getVar = 2},Var {getVar = 0},Var {getVar = 1}]]) :| [])}


