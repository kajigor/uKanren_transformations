module Transformer.ConsPD where


import           ConsPD.Residualization
import qualified ConsPD.Unfold          as ConsPD
import           Control.Monad          (guard)
import           Data.Maybe             (isJust, fromJust)
import qualified OCanrenize             as OC
import           Printer.Dot
import           Printer.ConsPDTree     ()
import           Purification
import           Residualization        (vident)
import           Syntax
import           System.FilePath        ((<.>), (</>))
import           System.Process         (system)
import           Text.Printf
import qualified Transformer.MkToProlog
import           Util.Miscellaneous     (escapeTick)
import           Util.File              (createDirRemoveExisting)
import NormalizedSyntax (makeNormal, normalizeProg)

data TransformResult = Result { original :: [Def]
                              , tree :: ConsPD.ConsPDTree
                              , simplifiedTree :: ConsPD.ConsPDTree
                              , names :: [X]
                              , beforePurification :: Maybe (G X, [X], [Def])
                              , purified :: Maybe (G X, [X], [Def])
                              }

type Transformer = Program -> (ConsPD.ConsPDTree, G S, [S])

runTransformation :: Program -> Transformer -> TransformResult
runTransformation goal@(Program original _) transformer =
  let transformed@(tree, logicGoal, names) = transformer goal in
  let namesX = vident <$> reverse names in
  let simplifiedTree = ConsPD.simplify tree in
  if ConsPD.noPrune tree
  then
    let residualized = residualize transformed in
    let beforePur = justTakeOutLetsProgram residualized namesX in
    let purified = purification (residualized, namesX) in
    Result original tree simplifiedTree namesX (Just beforePur) (Just purified)
  else
    Result original tree simplifiedTree namesX Nothing Nothing

toOcanren :: FilePath -> Program -> [String] -> IO ()
toOcanren fileName (Program defs goal) names =
  OC.topLevel fileName "topLevel" Nothing (goal, names, defs)

runConsPD l = Transformer.ConsPD.transform "test/out/consPD" Nothing (ConsPD.topLevel l)

runConsPD' :: [Char] -> FilePath -> Program -> IO ()
runConsPD' outDir = Transformer.ConsPD.transform outDir Nothing (ConsPD.topLevel (-1))

transform :: [Char] -> Maybe String -> (Program -> (ConsPD.ConsPDTree, G S, [S])) -> FilePath -> Program -> IO ()
transform outDir env function filename prg = do
  let norm = normalizeProg prg
  -- print norm

  let goal@(Program definitions _) = makeNormal prg
  let path = outDir </> filename
  createDirRemoveExisting path
  let consPdFile = path </> "conspd"

  Transformer.MkToProlog.transform (path </> "original.pl") definitions


  let result = runTransformation goal function

  writeFile (path </> "norm.txt") (show norm)
  toOcanren (path </> "original.ml") goal (names result)
  Transformer.MkToProlog.transform (path </> "original.pl") definitions
  printTree (path </> "tree.dot") (tree result)
  printTree (path </> "tree.after.dot") (simplifiedTree result)
  system (printf "dot -O -Tpdf %s/*.dot" (escapeTick path))

  guard (isJust $ beforePurification result)
  let ocamlCodeFileName = path </> filename <.> "before.ml"
  OC.topLevel ocamlCodeFileName "topLevel" env (fromJust $ beforePurification result)

  guard (isJust $ purified result )
  let pur@(goal', _, defs') = fromJust $ purified result
  let prog = Program defs' goal'
  Transformer.MkToProlog.transform (path </> filename <.> "pl") defs'
  writeFile (path </> filename <.> "pur") (show prog)
  let ocamlCodeFileName = path </> filename <.> "ml"
  OC.topLevel ocamlCodeFileName "topLevel" env pur
