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
import           System.Process                 (system)
import           Text.Printf
import           Util.Miscellaneous             (escapeTick)

transform env function filename goal = (do
  traceM filename
  let transformed@(tree, logicGoal, names) = function goal
  let tree' = NC.simplify tree
  -- traceM (printf "\n========================================\nBefore:\n%s\n\nAfter:\n%s\n========================================\n" (show tree) (show $ NC.simplify tree))

  let path = printf "test/out/nc/%s" filename
  exists <- doesDirectoryExist path
  if exists
  then removeDirectoryRecursive path
  else return ()
  createDirectoryIfMissing True path
  -- toOcanren (printf "%s/original.ml" path) goal (vident <$> names)
  printTree (printf "%s/tree.dot" path) tree
  printTree (printf "%s/tree.after.dot" path) tree'
  system (printf "dot -O -Tpdf %s/*.dot" (escapeTick path))
  guard (NC.noPrune tree)
  let prog = residualize transformed
  writeFile (printf "%s/%s.before.pur" path filename) (show prog)

  let beforePur = justTakeOutLetsProgram prog (vident <$> reverse names)
  let ocamlCodeFileName = printf "%s/%s.before.ml" path filename
  OC.topLevel ocamlCodeFileName "topLevel" env beforePur

  let pur@(goal', xs, defs') = purification (prog, vident <$> reverse names)
  -- let pur = (goal, xs, map (\(Def n as b) -> Def n as (E.closeFresh as b)) defs)
  let prog = Program defs' goal'
  writeFile (printf "%s/%s.pur" path filename) (show prog)
  let ocamlCodeFileName = printf "%s/%s.ml" path filename
  OC.topLevel ocamlCodeFileName "topLevel" env pur
  ) <|>
  return ()
