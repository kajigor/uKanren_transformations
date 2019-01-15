module CpdTest (tests) where

import Bool
import CPD
import Control.Monad
import Data.Maybe
import Data.List
import qualified Eval as E
import List
import Num
import Programs
import Purification
import SldTreePrinter
import Syntax
import Text.Printf
import Debug.Trace
import Prelude hiding (succ)

tests = do
  testEmbedding
  testSelect
  testTakingOutLets
  testPopingOutFreshes
  testNormalization
  testUnifyStuff
  testLocalControl
  printStuff

reportError :: Show a => String -> a -> a -> IO ()
reportError name expected actual =
  putStrLn $ printf "%s failed\nExpected: %s\nActual:   %s" name (show expected) (show actual)

assertCustom :: Show a => String -> (a -> a -> Bool) -> a -> a -> IO ()
assertCustom name check expected actual =
  unless (check expected actual) $ reportError name expected actual

anyAssertCustom :: Show a => String -> (a -> a -> Bool) -> [a] -> a -> IO ()
anyAssertCustom name check expected actual =
  mapM_ (\e -> assertCustom name check e actual) expected

manyAssertCustom :: Show a => String -> (a -> a -> Bool) -> [a] -> [a] -> IO ()
manyAssertCustom name check expected actual =
  unless (all (\a -> isJust $ find (check a) expected) actual) $ putStrLn (printf "%s failed\nExpected: %s\nActual:   %s" name (show expected) (show actual))

assert :: (Show a, Eq a) => String -> a -> a -> IO ()
assert name =
  assertCustom name (==)

manyAssert :: (Show a, Eq a) => String -> a -> (b -> b -> a) -> [(b, b)] -> IO ()
manyAssert name expected f =
  mapM_ (\(x, y) -> assert name expected (f x y))

printStuff = do
  printTree "sldDouble.dot" (topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])))
  printTree "sldAppNil.dot" $ topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"]))
  printTree "maxLengtho.dot" $ topLevel (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"]))
  printTree "maxo.dot" $ topLevel (maxo $ fresh ["x", "m"] (call "maxo" [V "x", V "m"]))

testEmbedding = do
  testHomeoTerm
  testHomeoGoal
  testHomeoConj
  testInstTerm
  testInstGoal
  testInstConj
  testEmbedGoal
  where
    x = V "x"
    y = V "y"
    z = V "z"
    v = V ""
    c = C ""
    n = C "n"
    m = C "m"
    f = Invoke "f"
    g = Invoke "g"
    testHomeoTerm = do
      manyAssert "homeo term" True homeo  [ (x, y)
                                          , (v, c [v])
                                          , (n [v], n [m [v]])
                                          ]
      manyAssert "homeo term" False homeo [ (c [], v)
                                          , (v, c [])
                                          , (c [v], n [m [v]])
                                          ]
    testHomeoGoal = do
      manyAssert "homeo goal" True homeo  [ (f [], f [])
                                          , (f [v], f [x])
                                          , (f [n [v]], f [m [n [v]]])
                                          , (f [m [v], n [v, v]], f [m [x], n [v, m [v]]])
                                          ]
      manyAssert "homeo goal" False homeo [ (f [], g [])
                                          , (f [c [v]], f [n [m [v]]])
                                          , (f [m [v], n [v, m []]], f [m [], n [v, v]])
                                          ]
    testHomeoConj = do
      manyAssert "homeo conj" True homeo  [ ([f []], [f []])
                                          , ([f [], g[]], [f [], f [], g []])
                                          , ([f [v]], [f [x]])
                                          , ([f [n [v]]], [f [m [n [v]]]])
                                          , ([f [m [v], n [v, v]]], [f [m [x], n [v, m [v]]]])
                                          ]
      manyAssert "homeo conj" False homeo [ ([f []], [g []])
                                          , ([f [c [v]]], [f [n [m [v]]]])
                                          ]
    testInstTerm = do
      manyAssert "isInst term" True isInst  [ (v, x)
                                            , (v, n [])
                                            , (n [], n [])
                                            , (n [x, y], n [y, x])
                                            , (n [x, y], n [x, x])
                                            , (n [x, m [x]], n [y, m [y]])
                                            ]

      manyAssert "isInst term" False isInst [ (n [], v)
                                            , (n [v], n [])
                                            , (n [x, x], n [y, x])
                                            , (n [x, m [x]], n [x, y])
                                            , (n [x, m [x]], n [x, m [y]])
                                            ]

    testInstGoal = do
      manyAssert "isInst goal" True isInst  [ (f [v], f [n [x]])
                                            , (f [x, y], f [y, x])
                                            , (f [x, y], f [x, x])
                                            ]

      manyAssert "isInst goal" False isInst [ (f [], g [])
                                            , (f [n [x, m [x]]], f [n [x, m [y]]])
                                            , (f [], f [v])
                                            , (f [v], f [])
                                            ]
    testInstConj = do
      manyAssert "isInst conj" True isInst  [ ([f [v]], [f [n [x]]])
                                            , ([f [x, y], g [v]], [f [x, x], g [x]])
                                            , ([f [x, y], g [y]], [f [x, x], g [x]])
                                            ]

      manyAssert "isInst conj" False isInst [ ([], [f []])
                                            , ([f [n [x, m [x]]]], [f [n [x, m [y]]]])
                                            , ([f [x, y], g [x]], [f [x, x], g [y]])
                                            ]

    testEmbedGoal = do
      manyAssert "embed goal" False embed [ (f [n [x, v], x, y], f [v, x, y])
                                          , (f [x, x], f [x, y])
                                          , (f [x, x, y, y], f [x, x, y, z])
                                          ]

      manyAssert "embed goal" True embed  [ (f [x, y], f [x, x])
                                          , (f [x, x, y, y], f [x, z, z, x])
                                          , (f [x, z, z, x], f [x, x, y, z])
                                          , (f [v, x, x], f [n [x, v], x, y])
                                          , (f [v, x, n [y, x]], f [v, x, n [y, x]])
                                          , (f [x, y, x], f [x, y, y])
                                          , (f [x, y, y], f [x, y, x])
                                          , (f [v, x, y], f [x, y, v]) -- variant
                                          , (f [v, x, y], f [n [x, v], x, y]) -- not a strict instance
                                          , (f [c [], m [x, x]], f [n [c []], m [x, y]])
                                          ]

testSelect = do
  testSelect1
  testSelect2
  where
    testSelect1 = do
      assert "select 0" (Just app00D) (select [app00D, app01D])
      assert "select 1" (Just app11D) (select [app10D, app11D])
      assert "select 2" (Just app21D) (select [app20D, app21D])
      where
        xs  = V 0
        ys  = V 1
        t   = V 2
        zs  = V 3
        r   = V 4
        xs' = V 5
        t'  = V 6
        h   = V 7
        r'  = V 8
        cons h t = C "cons" [h, t]
        app  x y z = Invoke "app"  [x, y, z]
        app00 = app xs ys t
        app01 = app t zs r
        app00D = Descend app00 []
        app01D = Descend app01 []
        app10 = app xs' ys t'
        app11 = app (cons h t') zs r
        app10D = Descend app10 [app00]
        app11D = Descend app11 []
        app20 = app10
        app21 = app t' zs r'
        app20D = Descend app20 [app00]
        app21D = Descend app21 [app11]
    testSelect2 = do
      assert "select 0" (Just max0D) (select [max0D, len0D])
      assert "select 1" (Just len1D) (select [max1D, len1D])
      assert "select 2" (Just len2D) (select [max2D, len2D])
      where
        n = V 0
        m = V 1
        l = V 2
        h = V 3
        k = V 4
        t = V 5
        x = V 6
        y = V 7
        z = V 8
        cons h t = C "Cons" [h, t]
        max' x y z = Invoke "max'" [x, y, z]
        len  x y   = Invoke "len"  [x, y]
        max0 = max' x n m
        len0 = len x l
        max0D = Descend max0 []
        len0D = Descend len0 []
        max1 = max' t n m
        len1 = len (cons h t) l
        max1D = Descend max1 [max0]
        len1D = Descend len1 []
        max2 = max' t n m
        len2 = len t k
        max2D = Descend max2 [max0]
        len2D = Descend len2 [len1]

testTakingOutLets = do
  assert "taking out lets 0" (uni0, [], []) (justTakeOutLets (uni0, []))
  assert "taking out lets 1" (uni1, [], [(defName, args0', uni0')]) (justTakeOutLets (flatlet, []))
  assert "taking out lets 2" (conj, [], [(defName, args0'', uni0''), (fedName, args2', disj')]) (justTakeOutLets (doublet, []))
  where
    x = V "x"
    y = V "y"
    z = V "z"
    y0 = V "y0"
    y1 = V "y1"
    y2 = V "y2"
    y3 = V "y3"
    y4 = V "y4"
    uni0 = x === y
    uni1 = y === z
    uni2 = x === z
    conj = uni0 &&& uni1
    disj = conj ||| uni2
    uni0' = y0 === y1
    uni0'' = y3 === y4
    uni1' = y1 === y2
    uni2' = y0 === y2
    conj' = uni0' &&& uni1'
    disj' = conj' ||| uni2'
    defName = "def"
    fedName = "fed"
    args0 = []
    args1 = ["x"]
    args0'  = ["y0", "y1"]
    args0'' = ["y3", "y4"]
    args1' = ["y2"]
    args2' = ["y1", "y2", "y0"]
    flatlet = Let (def defName args0 uni0) uni1
    doublet = Let (def defName args0 uni0) (Let (def fedName args1 disj) conj)

testPopingOutFreshes = do
  assert "popping freshes up 0" (callF x' y') ((\(x, _, _) -> x) $ E.preEval' E.env0 (fresh ["x", "y"] goal))
  assert "popping freshes up 1" (fLet (callF x' y')) ((\(x, _, y) -> x) $ E.preEval' E.env0 (fresh ["x", "y"] $ fLet goal))
  assert "popping freshes up 2" (body', reverse [0..4]) ((\(x, _, y) -> (x, y)) $ E.preEval' E.env0 (fresh ["m", "n"] body))
  where
    x = V "x"
    y = V "y"
    m = V "m"
    n = V "n"
    h = V "h"
    t = V "t"
    cS x = C "S" [x]
    cT x = C "T" [x]
    callF x y = Invoke "f" [x, y]
    callT x = Invoke "t" [x]
    gamma = []
    goal = callF x y
    body = fresh ["h"] (m === cS h &&& fresh ["t"] (n === cT t) &&& m === cS n) ||| fresh ["h"] (m === n &&& m === cT h &&& callT h)
    fLet = Let (def "f" ["m", "n"] body)
    x' = V 0
    y' = V 1
    body' = (V 0 === cS (V 2) &&& (V 1 === cT (V 3) &&& V 0 === cS (V 1))) ||| (V 0 === V 1 &&& (V 0 === cT (V 4) &&& callT (V 4)))

testNormalization = do
  assert "normalization 0" [[t === u]] (normalize (t === u))
  assert "normalization 1" [[f]] (normalize f)
  assert "normalization 2" [[f, g]] (normalize (f &&& g))
  assert "normalization 3" [[f], [g]] (normalize (f ||| g))
  assert "normalization 4" [[m], [f, g, t === u], [h, t === u]] (normalize (m ||| (f &&& g ||| h) &&& (t === u) ))
  where
    x = V 0
    y = V 1
    t = V 42
    u = V 13
    f = Invoke "f" []
    g = Invoke "g" []
    h = Invoke "h" []
    m = Invoke "m" []

testUnifyStuff = do
  assert "unifyStuff 0" (Just ([], E.s0)) (unifyStuff' [])
  assert "unifyStuff 1" (Just ([], [(0, y)])) (unifyStuff' [x === y, y === x])
  assert "unifyStuff 2" (Just ([], [(1, s), (0, y)])) (unifyStuff' [x === y, x === s])
  assert "unifyStuff 3" Nothing (unifyStuff' [x === s, x === t])
  assert "unifyStuff 4" Nothing (do (gs, state) <- unifyStuff' [x === s]
                                    unifyStuff state [x === t]
                                )
  assert "unifyStuff 5" (Just ([f x y, g x], [(13, x), (1, t), (0, y)])) (unifyStuff' [f x y, x === y, g x, t === y, x === u])
  assert "unifyStuff 6" (Just (inp, E.s0)) (unifyStuff' inp)
  where
    unifyStuff' = unifyStuff E.s0
    inp = [f x y, f u v, g x, g v, g s]
    x = V 0
    y = V 1
    u = V 13
    v = V 42
    s = C "s" []
    t = C "t" []
    f x y = Invoke "f" [x, y]
    g x = Invoke "g" [x]
    h = Invoke "h" []

testLocalControl = do
  manyAssertCustom "local control" isVariant [[app x y z], [app x y t, app t z r]] (leaves $ topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])))
  manyAssertCustom "local control" isVariant [[app x y z]] (leaves $ topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"])))
  assertCustom "local control" (\x y -> length x == length y) [] (leaves $ topLevel (doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [C "Stupid" [], V "y", V "z", V "r"])))
  manyAssertCustom "local control" isVariant [ [ maxo1 x zero z, lengtho x y ]
                                             , [ maxo1 (x % y) (succ z) r, lengtho y t ]
                                             , [ leo z u trueo, maxo1 (x % y) (succ (succ u)) r, lengtho y t ]
                                             , [ maxo1 (x % y) (succ (succ r)) t, lengtho y z]
                                             , [ gto z r trueo, maxo1 (x % y) (succ (succ z)) t, lengtho y u ]
                                             ]
                                             (leaves $ topLevel (maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"])))
  where
    app x y z = Invoke "appendo" [x, y, z]
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    lengtho x y = Invoke "lengtho" [x, y]
    gto x y z = Invoke "gto" [x, y, z]
    leo x y z = Invoke "leo" [x, y, z]
    x = V 0
    y = V 1
    t = V 2
    z = V 3
    r = V 4
    u = V 5
