module EvalApp where

import Syntax
import Program
import Stream
import Eval
import qualified Subst
import Program.List
import Program.Num
import Program.Bool
import Program.Sort
import Program.Programs
import Prelude hiding (succ)

reify :: Subst.Subst -> Ts -> Term S
reify s x@(V v) =
  case Subst.lookup v s of
    Nothing -> x
    Just t  -> reify s t
reify s (C n ts) = C n $ map (reify s) ts

toplevel :: Int -> (Term S -> String) -> Program G X -> [String]
toplevel n printer program =
  let stream = run program in
  map (\s -> printer $ reify s (V 0)) $ takeS n stream

addVar :: Program G String -> Program G String
addVar p@(Program defs goal) =
  let (vars, g) = topLevelFreshVars goal in
  if length vars <= 1
  then p
  else
    let newVar = concat vars in
    Program defs (fresh (newVar:vars) (V newVar === C "tuple" (map V vars)) &&& g)

runWithParser :: (t -> IO (Either String (Program G String))) -> t -> Int -> IO ()
runWithParser parser inputFile num = do
  res <- parser inputFile
  case res of
    Left err ->
      putStrLn err
    Right p ->
      mapM_ putStrLn (toplevel num show (addVar p))


main :: IO ()
main = do
  print (toplevel 1 show (Program lengtho (fresh ["l"] $ call "lengtho" [peanify 0 % (peanify 0 % (peanify 0 % (peanify 0 % (peanify 0 % nil)))), V "l"])))
  print (toplevel 1 show (Program lengtho' (fresh ["l"] $ call "lengtho'" [peanify 0 % (peanify 0 % (peanify 0 % (peanify 0 % (peanify 0 % nil)))), V "l"])))

main' :: IO ()
main' =
  do
    print (toplevel 1 bool (Program nando $ fresh ["q"] (call "nando" [falso, falso, V "q"])))
    print (toplevel 1 bool (Program nando $ fresh ["q"] (call "nando" [V "q", V "q", V "q"])))
    print (toplevel 1 show (Program nando $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "nando" [V "r", V "r", V "p"])))

    print (toplevel 1 bool (Program noto $ fresh ["q"] (call "noto" [falso, V "q"])))
    print (toplevel 1 bool (Program noto $ fresh ["q"] (call "noto" [V "q", V "q"])))
    print (toplevel 2 show (Program noto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "noto" [V "p", V "r"])))

    print (toplevel 3 show (Program oro $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "oro" [V "p", V "r", C "true" []])))
    print (toplevel 4 show (Program ando $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "ando" [V "p", V "r", C "false" []])))


    print (toplevel 4 show (Program leo $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "leo" [V "p", V "p", V "r"] )))
    print (toplevel 4 show (Program leo $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "leo" [V "p", V "r", C "false" []])))

    print (toplevel 4 show (Program geo $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "geo" [V "p", V "r", C "false" []])))

    print (toplevel 4 show (Program gto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "gto" [zero, V "p", V "r"] )))

    print (toplevel 4 show (Program gto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "gto" [V "p", V "p", V "r"] )))
    print (toplevel 4 show (Program gto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "gto" [V "p", V "r", C "false" []])))

    print (toplevel 4 show (Program gto $ fresh ["q"] (call "gto" [succ zero, zero, V "q"])))


    print (toplevel 4 show (Program lto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "lto" [V "p", V "r", C "false" []])))

    print (toplevel 4 list (Program singletono $ fresh ["q"] (call "singletono" [V "q", peanify 1])))
    print (toplevel 4 show (Program singletono $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "singletono" [V "p", V "r"])))

    print (toplevel 4 show (Program smallesto (fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "smallesto" [peanify 2 % (peanify 1 % nil), V "p", V "r"]))))

    print (toplevel 4 show (Program minmaxo (fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "minmaxo" [peanify 1, peanify 0, V "p", V "r"]))))

    print (toplevel 1 show (Program sorto (fresh ["q"] $ call "sorto" [peanify 3 % (peanify 1 % (peanify 0 % nil)), V "q"])))

    print (toplevel 10 show (Program eveno (fresh ["q"] $ call "eveno" [V "q"])))

    print (toplevel 10 show (Program oddo (fresh ["q"] $ call "oddo" [V "q"])))

    -- print (toplevel 1 show (Program sorto (fresh ["q"] $ call "sorto" [peanify 0 % (peanify 1 % (peanify 1 % (peanify 0 % nil))), V "q"])))


