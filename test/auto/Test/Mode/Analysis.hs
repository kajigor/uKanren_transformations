module Test.Mode.Analysis where

import           Control.Monad.State
import           Data.List           (subsequences)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Mode.Analysis
import           Mode.Inst
import qualified Mode.NormSyntax     as N
import           Mode.Pretty         (prettyString)
import           Mode.Syntax
import           Mode.Term
import           Mode.Toplevel
import           Program
import           Program.Num
import           Syntax
import           Test.Helper         ((@?=))

groundMode = Mode { before = Ground, after = Just Ground }
freeMode = Mode { before = Free, after = Nothing }
freeThenGroundMode = Mode { before = Free, after = Just Ground }

unit_unifyAnalysis :: IO ()
unit_unifyAnalysis = do
  let expected = N.Disj (N.Conj (N.Unif (Var (0, groundMode)) (FTVar (Var (1, freeThenGroundMode))) :| []) :| [])
  let actual = evalStateT (prioritizeGround (N.Disj (N.Conj (N.Unif (Var (0, groundMode)) (FTVar (Var (1, freeMode))) :| []) :| []))) emptyAnalyzeState :: Either ModeAnalysisError (N.Goal (Int, Mode))
  actual @?= Right expected

run (Right r) = print r
run (Left err) = putStrLn err

unit_analysis :: IO ()
unit_analysis = do
    mapM_ (go addoProg) (subsequences [0, 1, 2])
    putStrLn "\n==============\nmulo\n===============\n"
    mapM_ (go muloProg) (subsequences [0, 1, 2])
  where
    go prog ins = do
      putStrLn "\n----------------------------------\n"
      print ins
      run $ topLevel prog ins
      putStrLn "\n----------------------------------\n"
      putStrLn ""
    addoProg = Program addo (fresh ["x", "y", "z"] $ call "addo" [V "x", V "y", V "z"])
    muloProg = Program mulo (fresh ["x", "y", "z"] $ call "mulo" [V "x", V "y", V "z"])
