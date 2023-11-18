module Test.ConsPD where

import           Test.Helper            (test, test2)

import qualified ConsPD.Unfold          as ConsPD
import           Printer.ConsPDTree     ()
import           Program
import qualified Program.Bottles
import qualified Program.Bridge
import qualified Program.Bridge2
import qualified Program.Desert
import           Program.List           (appendo, maxLengtho, nil, revAcco, reverso, (%))
import           Program.Path
import           Program.Programs       (doubleAppendo, rep)
import qualified Program.Prop
import           Program.Stlc           (evalo)
import qualified Program.Unify
import qualified Subst
import           Syntax
import qualified Transformer.ConsPD     as ConsPD
import qualified Transformer.JustUnfold as JU
import qualified Transformer.MkToProlog as Mk2Pl

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
maxLen = Program maxLengtho $ fresh ["xs", "m", "l"] (call "maxLengtho" [V "xs", V "m", V "l"])
lambda = Program evalo $ fresh ["m", "n"] (call "evalo" [V "m", V "n"])

runJu = JU.transform

runConsPD = ConsPD.runConsPDNoGround

runRep = do
    runJu 100 "rep" rep
  where
    rep = Program Program.Programs.rep $ fresh ["n", "x"] (call "rep" [V "n", V "x"])

runAppendo = do
    mapM (\d -> runJu d ("app" ++ show d) app) [1..15]
    return ()
  where
    app = Program Program.List.appendo $ fresh ["x", "y", "z"] (call "appendo" [V "x", V "y", V "z"])

runProp = do
    -- runConsPD (-1) "prop"  prop
    -- runConsPD (-1) "prop1" prop1
    -- runConsPD (-1) "prop2" prop2
    -- runConsPD (-1) "prop3" prop3
    -- runConsPD (-1) "prop4" prop4
    -- runConsPD (-1) "prop_" prop'
    runConsPD (-1) "prop__" prop''
    -- runConsPD (-1) "prop1___" prop1'''
    -- runConsPD (-1) "prop2___" prop2'''
    runConsPD (-1) "propPlain" propPlain
    -- runConsPD (-1) "propPlain_" propPlain'
    -- runConsPD (-1) "prop__1" prop''1
    -- runConsPD (-1) "prop__2" prop''2
    -- runConsPD (-1) "prop__3" prop''3

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
    runConsPD (-1) "bottles" Program.Bottles.query
    -- runConsPD(-1) "bottlesQuery" Program.Bottles.query'
    -- runConsPD(-1) "fancyEq" Program.Bottles.queryEq

runBridge = do
    runConsPD (-1) "bridge" Program.Bridge.query
    runConsPD (-1) "bridge2" Program.Bridge2.query

runDesert = do
    runConsPD (-1) "desert"    Program.Desert.query
    runConsPD (-1) "desert_"   Program.Desert.query'
    runConsPD (-1) "desert__"  Program.Desert.query''
    runConsPD (-1) "desert___" Program.Desert.query'''
    runConsPD (-1) "desert1" Program.Desert.query1

runUnify = do
    runConsPD (-1) "unify" Program.Unify.query

runPath = do
    runConsPD (-1) "path" Program.Path.query1
    runConsPD (-1) "pathlen" Program.Path.querylength

unit_consPDTest = do
  -- runRep
  runAppendo
  -- runUnify
  -- runPath
  -- runProp
  -- runBottles

  -- -- runBridge
  -- -- runDesert
  -- runConsPD (-1) "da" dA

  -- runConsPD (-1) "rev" rev
  -- runConsPD (-1) "revAcco" revAcco'
  -- runConsPD (-1) "maxLen" maxLen
  -- -- runConsPD (55) "lambda" lambda

unit_printBottles = do
    Mk2Pl.transform "bottles.pl" Program.Bottles.bottles

unit_isConflicting = do
    test2' ConsPD.isConflicting [] [] False
    test2' ConsPD.isConflicting [(1, V 2)] [(2, V 3)] False
    test2' ConsPD.isConflicting [(1, V 2)] [(1, V 2)] False
    test2' ConsPD.isConflicting [(1, V 4)] [(1, V 3), (3, V 4)] False
    test2' ConsPD.isConflicting [(1, V 2)] [(1, V 2)] False
    test2' ConsPD.isConflicting [(1, V 2)] [(1, V 3)] False
    test2' ConsPD.isConflicting [(1, V 2)] [(1, C "" [])] False
    test2' ConsPD.isConflicting [(1, V 9),  (6, nil), (7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)]
                                [(2, V 10), (9, nil), (7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)]
                                False
    test2' ConsPD.isConflicting [(1, V 4), (0, nil)] [(2, V 3), (4, nil)] False
    test2' ConsPD.isConflicting [(1, V 4), (0, nil)] [(3, V 8 % V 10), (4, V 8 % V 9)] False
    test2' ConsPD.isConflicting [(4, V 5 % V 7), (0, V 5 % V 6)] [(3, V 8 % V 10), (4, V 8 % V 9)] False

    test2' ConsPD.isConflicting [(4, V 5 % V 7), (0, V 5 % V 6)] [(2, V 3), (4, nil)] True
    test2' ConsPD.isConflicting [(1, C "A" [])] [(1, C "B" [])] True
    test2' ConsPD.isConflicting [(1, C "A" [C "A" []])] [(1, C "A" [C "B" []])] True
    test2' ConsPD.isConflicting [(1, C "A" [])] [(1, C "A" [C "B" []])] True
  where
    test2' f g1 g2 r = test2 f (Subst.fromList g1) (Subst.fromList g2) r

unit_findConflicting = do
    test' ConsPD.findConflicting
          [[(1, V 2)], [(1, a)]]
          [[[(1, V 2)]], [[(1, a)]]]
    test' ConsPD.findConflicting
          [[(1, V 2)], [(1, a)], [(2, V 3)]]
          [[[(1, V 2)]], [[(1, a)]], [[(2, V 3)]]]
    test' ConsPD.findConflicting
          [[(1, V 2)], [(1, a), (2, b)], [(2, V 3)]]
          [[[(1, V 2)]], [[(1, a), (2, b)]], [[(2, V 3)]]]
    test' ConsPD.findConflicting
          [[(1, b)], [(1, a), (2, b)], [(2, V 3)]]
          [[[(1, b)], [(1, a), (2, b)]], [[(2, V 3)]]]
    test' ConsPD.findConflicting
          [[(1, b)], [(1, a), (2, b)], [(2, b)]]
          [[[(1, b)], [(1, a), (2, b)]], [[(2, b)]]]
    test' ConsPD.findConflicting
          [[(1, b)], [(1, a), (2, b)], [(2, a)]]
          [[[(1, b)], [(1, a), (2, b)], [(2, a)]]]
    test' ConsPD.findConflicting
          [[(1, b)], [(1, a), (2, c b)], [(2, c a)]]
          [[[(1, b)], [(1, a), (2, c b)], [(2, c a)]]]
    test' ConsPD.findConflicting
          [[(1, V 9),(6, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)], [(2, V 10),(9, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]]
          [[[(1, V 9),(6, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]], [[(2, V 10),(9, nil),(7, V 9),(5, V 8),(3, V 8 % V 10),(4, V 5 % V 7),(0, V 5 % V 6)]]]
  where
    a = C "a" []
    b = C "b" []
    c x = C "c" [x]
    test' f g1 g2 = test f (map Subst.fromList g1) (map (map Subst.fromList) g2)

unit_productList = do
    test ConsPD.productList [[]] ([] :: [[Int]])
    test ConsPD.productList ([] :: [[Int]]) [[]]
    test ConsPD.productList [[1,2,3]] [[1], [2], [3]]
    test ConsPD.productList [[1,2,3], [4], [5,6]] [[1,4,5], [1,4,6], [2,4,5], [2,4,6], [3,4,5], [3,4,6]]
    test ConsPD.productList [ [ ([], [(1, V 4), (0, nil)])
                              , ([appendo (V 6) (V 1) (V 7)], [(4, (V 5 % V 7)), (0, (V 5 % V 6))])
                              ]
                            , [ ([], [(2, V 3), (4, nil)])
                              , ([appendo (V 6) (V 2) (V 7)], [(3, (V 5 % V 7)), (4, (V 5 % V 6))])
                              ]
                            ]
                            [ [ ([], [(1, V 4), (0, nil)])
                              , ([], [(2, V 3), (4, nil)])
                              ]
                            , [ ([], [(1, V 4), (0, nil)])
                              , ([appendo (V 6) (V 2) (V 7)], [(3, (V 5 % V 7)), (4, (V 5 % V 6))])
                              ]
                            , [ ([appendo (V 6) (V 1) (V 7)], [(4, (V 5 % V 7)), (0, (V 5 % V 6))])
                              , ([], [(2, V 3), (4, nil)])
                              ]
                            , [ ([appendo (V 6) (V 1) (V 7)], [(4, (V 5 % V 7)), (0, (V 5 % V 6))])
                              , ([appendo (V 6) (V 2) (V 7)], [(3, (V 5 % V 7)), (4, (V 5 % V 6))])
                              ]
                            ]
  where
    appendo x y z = Invoke "appendo" [x, y, z]



unit_unifySubsts = do
  test' ConsPD.unifySubsts [] (Just [])
  test' ConsPD.unifySubsts [ [(1, V 4), (0, nil)]
                           , [(2, V 3), (4, nil)]
                           ]
                           (Just [(4, nil), (2, V 3), (1, V 4), (0, nil)])
  test' ConsPD.unifySubsts [ [(4, V 5 % V 7),  (0, V 5 % V 6)]
                           , [(3, V 8 % V 10), (4, V 8 % V 9)]
                           ]
                           (Just [(7, V 9), (5, V 8), (3, V 8 % V 10), (4, V 5 % V 7), (0, V 5 % V 6)])
  where
    test' f g1 g2 = test f (map Subst.fromList g1) (Subst.fromList <$> g2)
