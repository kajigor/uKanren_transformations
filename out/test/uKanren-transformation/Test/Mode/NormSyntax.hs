module Test.Mode.NormSyntax where

import           Data.List.NonEmpty
import           Def
import           Mode.NormSyntax
import           Mode.Syntax        (flatten)
import           Mode.Term
import           Program
import           Program.Num
import qualified Syntax             as S
import           Test.Helper        ((@?=), Assertion)
import           VarRename

runTest :: Program S.G S.X -> Program Goal S.S -> Assertion
runTest program expected =
  case uniquelyRenameVars program of
    Right (program, nextVar) -> do
      let flat = normalize $ flatten program nextVar
      flat @?= expected
    Left err -> fail $ "Unique rename failed\n" ++ err

unit_addo :: Assertion
unit_addo = do
  runTest
    (Program addo (S.fresh ["x", "y", "z"] $ S.call "addo" [S.V "x", S.V "y", S.V "z"]))
    (Program {getDefs = [Def {getName = "addo", getArgs = [0, 1, 2], getBody = Disj (Conj (Unif (Var { getVar = 0 } ) (FTCon "O" []) :| [Unif (Var {getVar = 2}) (FTVar (Var {getVar = 1}))]) :| [Conj (Unif (Var { getVar = 0 } ) (FTCon "S" [ Var {getVar = 3} ]) :| [Unif (Var { getVar = 2 } ) (FTCon "S" [ Var {getVar = 4} ]), Call NotDelayed "addo" [Var {getVar = 3},Var {getVar = 1},Var {getVar = 4}] ])])}], getGoal = Disj (Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| []) :| [])})

unit_normSyntax :: IO ()
unit_normSyntax = do
  runTest
    -- goal: addo x y z & (a z | b y)
    (Program [] (S.fresh ["x", "y", "z"] $ S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.call "b" [S.V "y"]]]))
    (Program {getDefs = [Def {getName = "topLevel0", getArgs = [1,2], getBody = Disj (Conj (Call NotDelayed "a" [Var {getVar = 2}] :| []) :| [Conj (Call NotDelayed "b" [Var {getVar = 1}] :| [])])}], getGoal = Disj (Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call NotDelayed "topLevel0" [Var {getVar = 1},Var {getVar = 2}]]) :| [])})

  runTest
    -- goal: addo x y z & (a z | addo x y z & (a z | b y))
    (Program [] (S.fresh ["x", "y", "z"] $ S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.unsafeConj [S.call "addo" [S.V "x", S.V "y", S.V "z"], S.unsafeDisj [S.call "a" [S.V "z"], S.call "b" [S.V "y"]]]]]))
    Program {getDefs = [Def {getName = "topLevel0", getArgs = [0,1,2], getBody = Disj (Conj (Call NotDelayed "a" [Var {getVar = 2}] :| []) :| [Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call NotDelayed "topLevel00" [Var {getVar = 1},Var {getVar = 2}]])])},Def {getName = "topLevel00", getArgs = [1,2], getBody = Disj (Conj (Call NotDelayed "a" [Var {getVar = 2}] :| []) :| [Conj (Call NotDelayed "b" [Var {getVar = 1}] :| [])])}], getGoal = Disj (Conj (Call NotDelayed "addo" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}] :| [Call NotDelayed "topLevel0" [Var {getVar = 0},Var {getVar = 1},Var {getVar = 2}]]) :| [])}

unit_clashingRelationNames :: Assertion
unit_clashingRelationNames = do
  let fcall = S.call "f" [S.V "x"]
  let disj = S.unsafeDisj [fcall, fcall]
  runTest
    -- f x = (f x | f x) & (f x | f x)
    -- f0 x = (f x | f x) & (f x | f x)
    -- ? f x
    (Program
      [ Def "f" ["x"] (S.unsafeConj [disj, disj])
      , Def "f0" ["x"] (S.unsafeConj [disj, disj])
      ]
      (S.fresh ["x"] fcall)
    )
    Program {
      getDefs =
        [ Def "f"   [0] (Disj (Conj (Call NotDelayed "f1"  [Var 0] :| [Call NotDelayed "f2" [Var 0]]) :| []))
        , Def "f0"  [0] (Disj (Conj (Call NotDelayed "f02" [Var 0] :| [Call NotDelayed "f03" [Var 0]]) :| []))
        , Def "f02" [0] (Disj (Conj (Call NotDelayed "f"   [Var 0] :| []) :| [Conj (Call NotDelayed "f" [Var 0] :| [])]))
        , Def "f03" [0] (Disj (Conj (Call NotDelayed "f"   [Var 0] :| []) :| [Conj (Call NotDelayed "f" [Var 0] :| [])]))
        , Def "f1"  [0] (Disj (Conj (Call NotDelayed "f"   [Var 0] :| []) :| [Conj (Call NotDelayed "f" [Var 0] :| [])]))
        , Def "f2"  [0] (Disj (Conj (Call NotDelayed "f"   [Var 0] :| []) :| [Conj (Call NotDelayed "f" [Var 0] :| [])]))
        ],
      getGoal = Disj (Conj (Call NotDelayed "f" [Var 0] :| []) :| [])}



unit_freshVarsInDisj :: Assertion
unit_freshVarsInDisj = do
  runTest
    -- f x y = fresh m, n in x == pair m n & (g y m | g y n)
    -- g x y = x == y
    -- ? f x y
    (Program
      [ Def "f" ["x", "y"] (S.fresh ["m", "n"] (S.unsafeConj [(S.===) (S.V "x") (S.C "pair" [S.V "m", S.V "n"]), S.unsafeDisj [S.call "g" [S.V "y", S.V "m"], S.call "g" [S.V "y", S.V "n"]]]))
      , Def "g" ["x", "y"] ((S.===) (S.V "x") (S.V "y"))
      ]
      (S.fresh ["x", "y"] (S.call "f" [S.V "x", S.V "y"])))
    Program {
      getDefs =
        [ Def "f" [0, 1] (Disj (Conj (Unif (Var 0) (FTCon "pair" [Var 2, Var 3]) :| [Call NotDelayed "f0" [Var 1, Var 2, Var 3]]) :| []))
        , Def "g" [0, 1] (Disj (Conj (Unif (Var 0) (FTVar $ Var 1) :| []) :| []))
        , Def "f0" [1, 2, 3] (Disj (Conj (Call NotDelayed "g" [Var 1, Var 2] :| []) :| [Conj (Call NotDelayed "g" [Var 1, Var 3] :| [])]))],
      getGoal = Disj (Conj (Call NotDelayed "f" [Var 0, Var 1] :| []) :| [])}
