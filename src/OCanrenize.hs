{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module OCanrenize where

import System.Process
import System.IO
import System.IO.Temp
import Data.Char
import Data.List (intercalate)
import Syntax
import Driving
import Residualize

class OCanren a where
  ocanren :: a -> String

instance OCanren String where
  ocanren = id

instance OCanren v => OCanren (Term v) where
  ocanren (V v)        = ocanren v
  ocanren (C "Nil" _) = "nil ()"
  ocanren (C "Cons" [h,t]) = ocanren h ++ " % " ++ ocanren t
  ocanren (C (f:o) ts) = "(" ++ (toLower f : o) ++ case ts of 
                                                     [] -> " ()" 
                                                     _  -> concat [' ' : ocanren t | t <- ts]
                         ++ ")"

instance OCanren v => OCanren (G v) where
  ocanren (t1 :=:  t2)  = "(" ++ ocanren t1 ++ " === " ++ ocanren t2 ++ ")"
  ocanren (g1 :/\: g2)  = "(" ++ ocanren g1 ++ " &&& " ++ ocanren g2 ++ ")"
  ocanren (g1 :\/: g2)  = "(" ++ ocanren g1 ++ " ||| " ++ ocanren g2 ++ ")"
  ocanren (Fresh x g )  = let (names, goal) = freshVars [x] g in "(" ++ "fresh (" ++ intercalate " " names ++ ") (" ++ ocanren goal ++ "))"
  ocanren (Invoke f ts) = "(" ++ f ++ concat [' ' : ocanren t | t <- ts] ++ ")"
  ocanren (Let (n, as, b) g) = 
    case n of 
      'f':_ -> "let rec " ++ n ++ concat [' ' : a | a <- as] ++ " = " ++ ocanren b ++ " in " ++ ocanren g
      _ -> "let rec " ++ n ++ concat [' ' : a | a <- as] ++ " = " ++ ocanren b

ocanrenize :: String -> [String] -> G X -> String
ocanrenize topLevelName args g = 
  "let " ++ topLevelName ++ " " ++ intercalate " " args ++ " = " ++ ocanren g

toOCanren filename topLevelName (tree, args) =
  do
    withSystemTempFile filename (\ tmp_name tmp -> 
                                   do
                                     hPutStrLn tmp (ocanrenize topLevelName args tree)
                                     hClose tmp
                                     file <- openFile filename WriteMode
                                     hPutStrLn file "open GT"
                                     hPutStrLn file "open MiniKanren"
                                     hPutStrLn file "open Std"
                                     hPutStrLn file "" 
                                     hClose file
                                     system $ "camlp5o pr_o.cmo " ++ tmp_name ++ " >> " ++ filename
                                     system $ "ocamlformat " ++ filename ++ " -m 160 -i"
                                     return ()
                                )

test = toOCanren "appendo2.ml" "appendo2" $ residualize tc 

test' = toOCanren "reverso.ml" "reverso" $ residualize tc'

test'' = toOCanren "revacco.ml" "revacco" $ residualize tc''

main = do 
  test 
  test'
  test''
