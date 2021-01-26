{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module OCanrenize where

import Control.Applicative ((<|>))
import Data.Char ( toLower )
import Data.Maybe (fromMaybe)
import Syntax ( freshVars, Def(..), G(..), Term(..), X )
import System.IO ( IOMode(WriteMode), hClose, openFile, hPutStrLn )
import System.IO.Temp ( withSystemTempFile )
import System.Process ( system )
import Text.Printf ( printf )
import Util.Miscellaneous ( parenthesize )

class OCanren a where
  ocanren :: a -> String

instance OCanren String where
  ocanren = id

instance OCanren v => OCanren (Term v) where
  ocanren (V v) = ocanren v
  ocanren (C name args) =
      let lcName = map toLower name in
      fromMaybe
        (consToString name args)
        (   getList lcName args
        <|> getSucc lcName args
        <|> getBool lcName args
        <|> getSome lcName args
        )
    where
      consToString name args =
        printf "(%s %s)" (parenthesize $ getConsName name) (printArgs (map ocanren args))

      getConsName name | isReserved name = printf "%s%s" name "_"
      getConsName (h : t) = toLower h : t
      getConsName "" = "emptyCons"

      isReserved name = name == "fst" || name == "snd"

      getList "nil" [] = Just "(List.nil ())"
      getList name [h, t] | name == "cons" || name == "%" =
        Just $ printf "(%s %% %s)" (parenthesize $ ocanren h) (parenthesize $ ocanren t)
      getList _ _ = Nothing

      getSucc name [] | name == "o" || name == "z" = Just "Nat.zero"
      getSucc name [x] | name == "s" = Just $ printf "(Nat.succ %s)" (parenthesize $ ocanren x)
      getSucc _ _ = Nothing

      getBool "true" [] = Just "!!true"
      getBool "false" [] = Just "!!false"
      getBool _ _ = Nothing

      getPair "pair" [f, s] =
        Just $ printf "(Pair.pair %s %s)" (parenthesize $ ocanren f) (parenthesize $ ocanren s)
      getPair _ _ = Nothing

      getSome "some" [x] =
        Just $ printf "(Option.some %s)" (parenthesize $ ocanren x)
      getSome "none" [] = Just "(Option.none ())"
      getSome _ _ = Nothing

instance OCanren (G X) where
  ocanren (t1 :=:  t2)  = printf "(%s === %s)" (ocanren t1) (ocanren t2)
  ocanren (g1 :/\: g2)  = printf "(%s &&& %s)" (ocanren g1) (ocanren g2)
  ocanren (g1 :\/: g2)  = printf "(%s ||| %s)" (ocanren g1) (ocanren g2)
  ocanren (Fresh x g )  = let (names, goal) = freshVars [x] g in printf "(fresh (%s) (%s))" (printArgs names) (ocanren goal)
  ocanren (Invoke "success" []) = "success"
  ocanren (Invoke "fail" []) = "fail"
  ocanren (Invoke f ts) = printf "(%s %s)" f (printArgs $ map ocanren ts)

printArgs [] = "()"
printArgs args = unwords $ map parenthesize args

ocanrenize :: String -> (G X, [String]) -> String
ocanrenize topLevelName (g, args) =
  printf "let %s %s = %s" topLevelName (printArgs args) (ocanren g)

ocanrenize' :: String -> (G X, [String], [Def]) -> String
ocanrenize' topLevelName input@(g, args, defs) =
    printf "let %s %s = %s %s" topLevelName (printArgs args) (printDefs defs) (ocanren g)
  where
    printFstDef (Def n as g) = printf "\n  let rec %s %s = %s" n (printArgs as) (ocanren g)
    printLastDefs [] = "\n  in "
    printLastDefs ((Def n [] g) : ds) =
      printLastDefs ds
    printLastDefs ((Def n as g) : ds) =
      printf "\n  and %s %s = %s %s " n (printArgs as) (ocanren g) $ printLastDefs ds

    printDefs []     = ""
    printDefs (d:ds) = (printFstDef d) ++ " " ++ (printLastDefs ds)

toOCanren = toOCanren' ocanrenize

topLevel = toOCanren' ocanrenize'

toOCanren' printer filename topLevelName environment prog = do
    let fn = filter (/= '/') filename
    withSystemTempFile fn (\ tmp_name tmp -> do
                                hPutStrLn tmp (printer topLevelName prog)
                                hClose tmp
                                printEnvironment filename environment
                                system $ "cat " ++ tmp_name ++ " >> " ++ filename
                                return ()
                          )
  where
    printEnvironment filename (Just env) = do
        file <- openFile filename WriteMode
        hPutStrLn file env
        hClose file
    printEnvironment filename Nothing = do
        file <- openFile filename WriteMode
        hPutStrLn file "open GT"
        hPutStrLn file "open OCanren"
        hPutStrLn file "open OCanren.Std"
        hPutStrLn file "open Helper"
        hPutStrLn file ""
        hClose file
