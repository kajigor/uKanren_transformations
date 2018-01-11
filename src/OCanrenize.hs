{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module OCanrenize where

import System.Process
import System.IO
import System.IO.Temp
import Data.Char
import Syntax
import Test
import Driving
import Residualize

class OCanren a where
  ocanren :: a -> String

instance OCanren String where
  ocanren = id

instance OCanren v => OCanren (Term v) where
  ocanren (V v)        = ocanren v
  ocanren (C (f:o) ts) = "(" ++ (toLower f : o) ++ case ts of 
                                                     [] -> " ()" 
                                                     _  -> concat [' ' : ocanren t | t <- ts]
                         ++ ")"

instance OCanren v => OCanren (G v) where
  ocanren (t1 :=:  t2)  = "(" ++ ocanren t1 ++ " === " ++ ocanren t2 ++ ")"
  ocanren (g1 :/\: g2)  = "(" ++ ocanren g1 ++ " &&& " ++ ocanren g2 ++ ")"
  ocanren (g1 :\/: g2)  = "(" ++ ocanren g1 ++ " ||| " ++ ocanren g2 ++ ")"
  ocanren (Fresh x g )  = "(" ++ "fresh (" ++ x ++ ") " ++ ocanren g ++ ")"
  ocanren (Invoke f ts) = "(" ++ f ++ concat [' ' : ocanren t | t <- ts] ++ ")"
  ocanren (Let (n, as, b) g) = "let rec " ++ n ++ concat [' ' : a | a <- as] ++ " = " ++ ocanren b ++ " in " ++ ocanren g

ocanrenize :: G X -> String
ocanrenize = ocanren

toOCanren filename tree =
  do
    withSystemTempFile filename (\ tmp_name tmp -> 
                                   do
                                     hPutStrLn tmp (ocanren tree)
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

test = toOCanren "appendo2.ml" $ residualize tc

test' = toOCanren "reverso.ml" $ residualize tc'

test'' = toOCanren "revacco.ml" $ residualize tc''
