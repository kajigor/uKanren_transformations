module Main where

import           NonConjunctive.Unfold
import qualified Program.Bottles
import qualified Program.Bridge
import qualified Program.Bridge2
import qualified Program.Desert
import           Program.List           (appendo, maxLengtho, nil, revAcco,
                                         reverso, (%))
import           Program.Path
import           Program.Programs       (doubleAppendo, rep)
import qualified Program.Prop
import qualified Program.Sort
import           Program.Stlc           (evalo)
import qualified Program.Typing
import qualified Program.L
import qualified Program.Unify
import           Syntax
import qualified Transformer.JustUnfold
import qualified Transformer.NonConj
import qualified Transformer.PrologToMk

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
maxLen = Program maxLengtho $ fresh ["xs", "m", "l"] (call "maxLengtho" [V "xs", V "m", V "l"])
lambda = Program evalo $ fresh ["m", "n"] (call "evalo" [V "m", V "n"])

runJu = Transformer.JustUnfold.transform

runNc = Transformer.NonConj.runNc

runRep = do
    runJu 100 "rep" rep
  where
    rep = Program Program.Programs.rep $ fresh ["n", "x"] (call "rep" [V "n", V "x"])

runProp = do
    -- runNc (-1) "prop"  prop
    -- runNc (-1) "prop1" prop1
    -- runNc (-1) "prop2" prop2
    -- runNc (-1) "prop3" prop3
    -- runNc (-1) "prop4" prop4
    -- runNc (-1) "prop_" prop'
    runNc (-1) "prop__" prop''
    -- runNc (-1) "prop1___" prop1'''
    -- runNc (-1) "prop2___" prop2'''
    runNc (-1) "propPlain" propPlain
    -- runNc (-1) "propPlain_" propPlain'
    -- runNc (-1) "prop__1" prop''1
    -- runNc (-1) "prop__2" prop''2
    -- runNc (-1) "prop__3" prop''3

  where
    -- won't terminate: accumulator in assoco
    prop       = Program.Prop.query
    -- won't terminate: accumulator in assoco
    prop1      = Program.Prop.query1
    -- won't terminate: accumulator in assoco
    prop2      = Program.Prop.query2
    prop3      = Program.Prop.query3
    prop4      = Program.Prop.query4
    prop'      = Program.Prop.query'
    prop''     = Program.Prop.query''
    prop1'''   = Program.Prop.query1'''
    prop2'''   = Program.Prop.query2'''
    propPlain  = Program.Prop.plainQuery
    propPlain' = Program.Prop.plainQuery'
    prop''1    = Program.Prop.query''1
    prop''2    = Program.Prop.query''2
    prop''3    = Program.Prop.query''3

runBottles = do
    runNc (-1) "bottles" Program.Bottles.query
    -- runNc(-1) "bottlesQuery" Program.Bottles.query'
    -- runNc(-1) "fancyEq" Program.Bottles.queryEq

runBridge = do
    runNc (-1) "bridge" Program.Bridge.query
    -- runNc (-1) "bridge2" Program.Bridge2.query

runDesert = do
    -- runNc (-1) "desert"    Program.Desert.query
    -- runNc (-1) "desert_"   Program.Desert.query'
    -- runNc (-1) "desert__"  Program.Desert.query''
    -- runNc (-1) "desert___" Program.Desert.query'''
    runNc (-1) "desert1" Program.Desert.query1

runUnify = do
    runNc (-1) "unify" Program.Unify.query

runPath = do
    runNc (-1) "path" Program.Path.query1
    runNc (-1) "pathlen" Program.Path.querylength

runAppendo = do
    mapM (\d -> runJu d ("app" ++ show d) app) [1..15]
    return ()
  where
    app = Program Program.List.appendo $ fresh ["x", "y", "z"] (call "appendo" [V "x", V "y", V "z"])

runSort = do
    runNc (-1) "sort" Program.Sort.query

runTyping = do
    runNc (-1) "type" Program.Typing.query

runL = do
    runNc (-1) "llang" Program.L.query


main :: IO ()
main = do
    -- runL
    Transformer.PrologToMk.transform "test/out/nc/llang/ecce.pl"
    -- runTyping
    -- runSort

    -- runBridge
    -- runDesert
    -- runNc (-1) "maxlen" maxLen
--   runAppendo
--   runBottles

