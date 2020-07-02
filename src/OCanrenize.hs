{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module OCanrenize where

import System.Process
import System.IO
import System.IO.Temp
import Data.Char
import Syntax
import Text.Printf

class OCanren a where
  ocanren :: a -> String

instance OCanren String where
  ocanren = id

instance {- OCanren v => -} OCanren (Term X) where
  -- ocanren (V v@('q':_)) = printf "(Obj.magic %s)" (ocanren v)
  ocanren (V v)        = ocanren v
  ocanren (C nil _) | map toLower nil == "nil"  = "(Std.List.nil ())"
  -- ocanren (C cons [h,t]) | map toLower cons == "cons" = printf "(%s %% %s)" (ocanren h) (ocanren t)
  -- ocanren (C "%"    [h,t]) = printf "(%s %% %s)" (ocanren h) (ocanren t)
  ocanren (C cons [h,t]) | map toLower cons == "cons" = printf "(Std.(%%) (%s) (%s))" (ocanren h) (ocanren t)
  ocanren (C "%" [h, t]) = printf "(Std.(%%) (%s) (%s))" (ocanren h) (ocanren t)
  ocanren (C "O" []) = "Std.Nat.zero"
  ocanren (C "S" [x]) = printf "(Std.Nat.succ (%s))" (ocanren x)

  ocanren (C "o" []) = "(o ())"
  ocanren (C "s" [x]) = printf "(s (%s))" (ocanren x)


  -- ocanren (C "o" []) = "Std.Nat.zero"
  -- ocanren (C "s" [x]) = printf "Std.Nat.succ (%s)" (ocanren x)
  ocanren (C "z" []) = "Nat.zero"

  ocanren (C "pair" [x,y]) = printf "(Pair.pair (%s) (%s))" (ocanren x) (ocanren y)
  ocanren (C "none" []) = "(Option.none ())"
  ocanren (C "some" [x]) = printf "(Option.some (%s))" (ocanren x)


  ocanren (C "fst" []) = "(fst_ ())"
  ocanren (C "snd" []) = "(snd_ ())"
  ocanren (C "fill" []) = "(fill ())"
  ocanren (C "empty" []) = "(empty ())"
  ocanren (C "pour" []) = "(pour ())"


  ocanren (C "true" []) = printf "!!true"
  ocanren (C "false" []) = printf "!!false"
  ocanren (C "ltrue" []) = printf "ltrue ()"
  ocanren (C "lfalse" []) = printf "lfalse ()"
  -- ocanren (C "o" []) = "o ()"
  -- ocanren (C "s" [x]) = printf "s (%s)" (ocanren x)
  -- ocanren (C "z" []) = "z ()"
  ocanren (C (f:o) ts) = (toLower f : o) ++ ' ' : printArgs (map ocanren ts)

instance {-OCanren v =>-} OCanren (G X) where
--ocanren (t1 :=:  t2)  = printf "(print_string \"%s === %s\\n\"; %s === %s)" (ocanren t1) (ocanren t2) (ocanren t1) (ocanren t2)
  ocanren (t1 :=:  t2)  = printf "(%s === %s)" (ocanren t1) (ocanren t2)
  ocanren (g1 :/\: g2)  = printf "(%s &&& %s)" (ocanren g1) (ocanren g2)
  ocanren (g1 :\/: g2)  = printf "(%s ||| %s)" (ocanren g1) (ocanren g2)
  ocanren (Fresh x g )  = let (names, goal) = freshVars [x] g in printf "(fresh (%s) (%s))" (printArgs names) (ocanren goal)
--ocanren (Invoke f ts) = printf "(print_string \"%s\\n\";%s)" (f ++ concat [' ' : ocanren t | t <- ts]) (f ++ concat [' ' : ocanren t | t <- ts])
  ocanren (Invoke "success" []) = "success"
  ocanren (Invoke "fail" []) = "fail"
  ocanren (Invoke f ts) = printf "(%s %s)" f (printArgs $ map ocanren ts)
  ocanren (Let (Def n as b) g) = printf "let rec %s %s = %s in %s" n (printArgs as) (ocanren b) (ocanren g)


-- instance {-OCanren v =>-} OCanren (G X) where
-- --ocanren (t1 :=:  t2)  = printf "(print_string \"%s === %s\\n\"; %s === %s)" (ocanren t1) (ocanren t2) (ocanren t1) (ocanren t2)
--   ocanren (t1 :=:  t2)  = printf "(%s === %s)" (ocanren t1) (ocanren t2)
--   ocanren (g1 :/\: g2)  = printf "defer (%s &&& %s)" (ocanren g1) (ocanren g2)
--   ocanren (g1 :\/: g2)  = printf "defer (%s ||| %s)" (ocanren g1) (ocanren g2)
--   ocanren (Fresh x g )  = let (names, goal) = freshVars [x] g in printf "(fresh ((%s)) (%s))" (printArgs names) (ocanren goal)
-- --ocanren (Invoke f ts) = printf "(print_string \"%s\\n\";%s)" (f ++ concat [' ' : ocanren t | t <- ts]) (f ++ concat [' ' : ocanren t | t <- ts])
--   ocanren (Invoke f ts) = printf "defer (%s %s)" f (printArgs $ map ocanren ts)
--   ocanren (Let (Def n as b) g) = printf "let rec %s %s = %s in %s" n (printArgs as) (ocanren b) (ocanren g)

printArgs [] = "()"
printArgs args = unwords $ map (\x -> if ' ' `elem` x then printf "(%s)" x else x ) args

ocanrenize :: String -> (G X, [String]) -> String
ocanrenize topLevelName (g, args) =
  printf "let %s %s = %s" topLevelName (printArgs args) (ocanren g)

ocanrenize' :: String -> (G X, [String], [Def]) -> String
ocanrenize' topLevelName input@(g, args, defs) =
    printf "let %s %s = %s %s" topLevelName (printArgs args) (printDefs defs) (ocanren g)
  where
    printFstDef (Def n as g) = printf "let rec %s %s = %s" n (printArgs as) (ocanren g)
    printLastDefs [] = "in "
    printLastDefs ((Def n [] g) : ds) =
      printLastDefs ds
    printLastDefs ((Def n as g) : ds) =
      printf "and %s %s = %s %s " n (printArgs as) (ocanren g) $ printLastDefs ds

    printDefs []     = ""
    printDefs (d:ds) = (printFstDef d) ++ " " ++ (printLastDefs ds)

toOCanren = toOCanren' ocanrenize

topLevel = toOCanren' ocanrenize'

toOCanren' printer filename topLevelName environment prog =
  do
    let fn = filter (/= '/') filename
    withSystemTempFile fn (\ tmp_name tmp ->
                              do
                                hPutStrLn tmp (printer topLevelName prog)
                                hClose tmp
                                printEnvironment filename environment
                                system $ "cat " ++ tmp_name ++ " >> " ++ filename
                                --system $ "camlp5o pr_o.cmo " ++ tmp_name ++ " >> " ++ filename
                                -- system $ "ocamlformat --enable-outside-detected-project " ++ filename ++ " -m 160 -i"
                                return ()
                          )
  where
    printEnvironment filename (Just env) =
      do
        file <- openFile filename WriteMode
        hPutStrLn file env
        hClose file
    printEnvironment filename Nothing =
      do
        file <- openFile filename WriteMode
        hPutStrLn file "open GT"
        hPutStrLn file "open OCanren"
        hPutStrLn file "open OCanren.Std"
        hPutStrLn file "open Helper"
        hPutStrLn file ""
        hClose file
