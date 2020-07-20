module Transformer.NonConj where


import           Control.Applicative            ((<|>))
import           Control.Monad                  (guard)
import           Debug.Trace                    (traceM)
import           NonConjunctive.Residualization
import qualified NonConjunctive.Unfold          as NC
import qualified OCanrenize                     as OC
import           Printer.Dot
import           Printer.NCTree                 ()
import           Purification
import           Residualize                    (vident)
import           Syntax
import           System.Directory
import           System.FilePath                ((</>), (<.>))
import           System.Process                 (system)
import           Text.Printf
import qualified Transformer.MkToProlog
import           Util.Miscellaneous             (escapeTick)


toOcanren fileName (Program defs goal) names =
  OC.topLevel fileName "topLevel" Nothing (goal, names, defs)

runNc l = Transformer.NonConj.transform "test/out/nc" True Nothing (NC.nonConjunctive l)

transform dirName cleanDir env function filename goal@(Program definitions _) = (do
  let transformed@(tree, logicGoal, names) = function goal
  let tree' = NC.simplify tree
  -- traceM (printf "\n========================================\nBefore:\n%s\n\nAfter:\n%s\n========================================\n" (show tree) (show $ NC.simplify tree))

  let path = dirName </> filename
  exists <- doesDirectoryExist path
  if cleanDir && exists
  then removeDirectoryRecursive path
  else return ()
  createDirectoryIfMissing True path
  toOcanren (path </> "original.ml") goal (vident <$> reverse names)
  Transformer.MkToProlog.transform (path </> "original.pl") definitions
  printTree (path </> "tree.dot") tree
  printTree (path </> "tree.after.dot") tree'
  system (printf "dot -O -Tpdf %s/*.dot" (escapeTick path))
  guard (NC.noPrune tree)
  let prog = residualize transformed
  writeFile (path </> (printf "%s.before.pur" filename)) (show prog)

  let beforePur = justTakeOutLetsProgram prog (vident <$> reverse names)
  let ocamlCodeFileName = path </> (printf "%s.before.ml" filename)
  OC.topLevel ocamlCodeFileName "topLevel" env beforePur

  let pur@(goal', xs, defs') = purification (prog, vident <$> reverse names)
  -- let pur = (goal, xs, map (\(Def n as b) -> Def n as (E.closeFresh as b)) defs)
  let prog = Program defs' goal'
  Transformer.MkToProlog.transform (path </> filename <.> "pl") defs'
  writeFile (path </> filename <.> "pur") (show prog)
  let ocamlCodeFileName = path </> filename <.> "ml"
  OC.topLevel ocamlCodeFileName "topLevel" env pur
  ) <|>
  return ()
