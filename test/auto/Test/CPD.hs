module Test.CPD where

import           Test.Helper (manyAssert, test, test2, assertCustom, manyAssertCustom)

import           Control.Monad
import qualified CPD.GlobalControl        as GC
import           CPD.LocalControl
import           CPD.Residualization
import           Data.Foldable            (for_)
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.Set                 as Set
import           Debug.Trace
import qualified Eval                     as E
import qualified OCanrenize               as OC
import           Prelude                  hiding (succ)
import           Printer.Dot
import           Printer.GlobalTree
import           Printer.SldTree
import           Program.Bool
import qualified Program.Bottles
import           Program.Bridge
import qualified Program.Desert
import           Program.List
import           Program.LogicInterpreter
import           Program.Num
import           Program.Path             hiding (elem)
import           Program.Programs
import           Program.Prop
import           Program.Sample1
import           Program.Sort
import           Program.SpecialProp
import qualified Program.Sudoku4x4
import           Program.Unify
import           Purification
import           Residualize
import           Syntax
import           System.Directory
import           Text.Printf
import           Unfold
import           Util.ConjRetriever
import           Util.Miscellaneous
import           Embed
import qualified Subst
import           System.CPUTime
import           System.IO
import           System.Process           (system)
import           System.TimeIt


unit_select = do
    testSelect1
    testSelect2
  where
    testSelect1 = do
      test select [app00D, app01D] (Just app00D)
      test select [app10D, app11D] (Just app11D)
      test select [app20D, app21D] (Just app21D)
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
      test select [max0D, len0D]  (Just max0D)
      test select [max1D, len1D]  (Just len1D)
      test select [max2D, len2D]  (Just len2D)
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

unit_popingOutFreshes = do
    test (fst3 . E.preEval E.env0) (fresh ["x", "y"] goal) (callF x' y')
    test (fst3 . E.preEval (E.gammaFromDefs [fDef])) (fresh ["x", "y"] $ goal) (callF x' y')
    test ((\(x, _, y) -> (x, y)) . E.preEval E.env0) (fresh ["m", "n"] body) (body', reverse [0..4])
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
    fDef = Def "f" ["m", "n"] body
    x' = V 0
    y' = V 1
    body' = (V 0 === cS (V 2) &&& (V 1 === cT (V 3) &&& V 0 === cS (V 1))) ||| (V 0 === V 1 &&& (V 0 === cT (V 4) &&& callT (V 4)))

unit_normalization = do
    test normalize (t === u) [[t === u]]
    test normalize f [[f]]
    test normalize (f &&& g) [[f, g]]
    test normalize (f ||| g) [[f], [g]]
    test normalize (m ||| (f &&& g ||| h) &&& (t === u) ) [[m], [f, g, t === u], [h, t === u]]
  where
    x = V 0
    y = V 1
    t = V 42
    u = V 13
    f = Invoke "f" []
    g = Invoke "g" []
    h = Invoke "h" []
    m = Invoke "m" []

unit_unifyStuff = do
    test unifyStuff' [] (Just ([], Subst.empty))
    test unifyStuff' [x === y, y === x] (Just ([], Map.fromList [(0, y)]))
    test unifyStuff' [x === y, x === s] (Just ([], Map.fromList [(1, s), (0, y)]))
    test unifyStuff' [f x y, x === y, g x, t === y, x === u] (Just ([f x y, g x], Map.fromList [(13, x), (1, t), (0, y)]))
    test unifyStuff' inp (Just (inp, Subst.empty))
    test unifyStuff' [x === s, x === t] Nothing
  where
    unifyStuff' = unifyStuff Subst.empty
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

unit_unifySubsts = do
    test2' E.unifySubsts [] [] (Just [])
    test2' E.unifySubsts [(1, V 2)] [] Nothing
    test2' E.unifySubsts [(1, C "d" [])] [(1, C "c" [])] Nothing
  where
    test2' f x y z = test2 f (Map.fromList x) (Map.fromList y) (Map.fromList <$> z)

-- unit_localControl = do
--   manyAssertCustom "local control" isVariant [[app x y z], [app x y t, app t z r]] (leaves $ topLevel (Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])))
--   manyAssertCustom "local control" isVariant [[app x y z]] (leaves $ topLevel (Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [nil, V "y", V "z", V "r"])))
--   manyAssertCustom "local control" isVariant [ [ maxo1 x zero z, lengtho x y ]
--                                              , [ maxo1 (x % y) (succ z) r, lengtho y t ]
--                                              , [ leo z u trueo, maxo1 (x % y) (succ (succ u)) r, lengtho y t ]
--                                              , [ maxo1 (x % y) (succ (succ r)) t, lengtho y z]
--                                              , [ gto z r trueo, maxo1 (x % y) (succ (succ z)) t, lengtho y u ]
--                                              ]
--                                              (leaves $ topLevel (Program maxLengtho $ fresh ["x", "l", "m"] (call "maxLengtho" [V "x", V "l", V "m"])))
--   where
--     app x y z = Invoke "appendo" [x, y, z]
--     maxo1 x y z = Invoke "maxo1" [x, y, z]
--     lengtho x y = Invoke "lengtho" [x, y]
--     gto x y z = Invoke "gto" [x, y, z]
--     leo x y z = Invoke "leo" [x, y, z]
--     x = V 0
--     y = V 1
--     t = V 2
--     z = V 3
--     r = V 4
--     u = V 5

unit_mcs = do
    test mcs [p x y, p t u, q x, q v] [[p x y, q x], [p t u], [q v]]
    test mcs ([] :: [G X]) []
    test mcs [p x y, p x z, p y z] [[p x y, p x z, p y z]]
    test mcs [p x y, p x y, p x y, p z y, q x, p t u, p u v, p v t, q y] [[p x y, p x y, p x y, p z y, q x, q y], [p t u, p u v, p v t]]
  where
    p x y = Invoke "p" [x, y]
    q x = Invoke "q" [x]
    x = V "x"
    y = V "y"
    z = V "z"
    t = V "t"
    u = V "u"
    v = V "v"

unit_msgExists = do
    test2 msgExists [] [f] False
    test2 msgExists [f] [] False
    test2 msgExists [f, f] [f, g] False
    test2 msgExists [f, g, f] [f, f, g] False
    test2 msgExists [f] [g] False
    test2 msgExists [h (m x y) x, h y x] [h x y, h (n (m x y)) z] True
    test2 msgExists [f, g] [f, g] True
    test2 msgExists [f] [f] True
    test2 msgExists [] [] True
  where
    f = Invoke "f" []
    g = Invoke "g" []
    h x y = Invoke "h" [x, y]
    k x = Invoke "k" [x]
    true = C "True" []
    false = C "False" []
    m x y = C "m" [x, y]
    n x = C "n" [x]
    x = V "x"
    y = V "y"
    z = V "z"

unit_subconjs = do
    test2 (\x y -> length $ subconjs x y) [f, g, f] 2 3
    test2 (\x y -> length $ subconjs x y) [f, g, f, f, f] 3 10
    test2 (\x y -> length $ subconjs x y) [f] 2 0
    test2 (\x y ->
              all (`elem` [ [f, g, l]
                          , [f, g, f]
                          , [f, g, p]
                          , [f, l, f]
                          , [f, l, p]
                          , [f, f, p]
                          , [g, l, f]
                          , [g, l, p]
                          , [g, f, p]
                          , [l, f, p]
                          ]) (subconjs x y))
          [f, g, l, f, p]
          3
          True
  where
    f = Invoke "f" []
    g = Invoke "g" []
    h x y = Invoke "h" [x, y]
    x = V "x"
    y = V "y"
    m x = C "m" [x]
    l = h x y
    p = h (m x) x

unit_complementSubconjs = do
    test2 complementSubconjs [f, g, l, f, p] [f, g, l, f, p]  []
    test2 complementSubconjs [f, g, l] [f, g, l, f, p]  [f, p]
    test2 complementSubconjs [f, g, f] [f, g, l, f, p]  [l, p]
    test2 complementSubconjs [f, g, p] [f, g, l, f, p]  [l, f]
    test2 complementSubconjs [f, l, f] [f, g, l, f, p]  [g, p]
    test2 complementSubconjs [f, l, p] [f, g, l, f, p]  [g, f]
    test2 complementSubconjs [f, f, p] [f, g, l, f, p]  [g, l]
    test2 complementSubconjs [g, l, p] [f, g, l, f, p]  [f, f]
    test2 complementSubconjs [g, f, p] [f, g, l, f, p]  [f, l]
    test2 complementSubconjs [l, f, p] [f, g, l, f, p]  [f, g]
  where
    f = Invoke "f" []
    g = Invoke "g" []
    h x y = Invoke "h" [x, y]
    x = V "x"
    y = V "y"
    m x = C "m" [x]
    l = h x y
    p = h (m x) x

unit_minimallyGeneral = do
    test minimallyGeneral' [[f x y], [f x x]] [f x x]
    test minimallyGeneral' [[f x x], [f x y]] [f x x]
    test minimallyGeneral' [[g x x y], [g x y y], [g x y x], [g x y z]] [g x x y]
    test minimallyGeneral' [[g x y z], [g x y y], [g x y x], [g x x y]] [g x y y]
    test minimallyGeneral' [[f x y, g x y z], [f x z], [f x x], [f x x, g x y z]] [f x y, g x y z] -- y and x are linked and the selected one is the firs
    test minimallyGeneral' [[f x z], [f x x], [f x x, g y y z], [f x y, g z t u]] [f x x]
    test minimallyGeneral' [[f x z, g x y z], [f x x, g y y z], [f x y, g z t u]] [f x z, g x y z]
    test minimallyGeneral' [[f x x, g y y z], [f x y, g z t u], [f x z, g x y z]] [f x x, g y y z]
    test minimallyGeneral' [[p u y, q y z], [p x u, q y z]] [p u y, q y z]
    test minimallyGeneral' [[p x u, q y z], [p u y, q y z]] [p u y, q y z]
  where
    f x y = Invoke "f" [x, y]
    g x y z = Invoke "g" [x, y, z]
    p x y = Invoke "p" [x, y]
    q x y = Invoke "q" [x, y]
    x = V "x"
    y = V "y"
    z = V "z"
    t = V "t"
    u = V "u"

    minimallyGeneral' = fst . minimallyGeneral . map (\x -> (x, Subst.empty))

testSplit = do -- TODO more tests
  assertCustom "split 0" checkVariant ([f x x], [g x]) (fst3 $ split [2..] [f x x] [f x x, g x] )
  assertCustom "split 1" checkVariant ([f x x], [g x]) (fst3 $ split [2..] [f x x] [g x, f x x] )
  assertCustom "split 2" checkVariant ([f x z], [g x]) (fst3 $ split [2..] [f x y] [g x, f x x] )
  assertCustom "split 3" checkVariant ([maxo1 (v150 % v153) (s v152) v1, lengtho v153 v154], [leo v121 v122])
                                      (fst3 $
                                       split [150..]
                                             [maxo1 (v51 % v52) (s v33) v1, lengtho v52 v53]
                                             [leo v121 v122, maxo1 (v126 % v127) (s (s (s (s (s v122))))) v1, lengtho v127 v128])
  assertCustom "split 4" checkVariant ([maxo1 v50 (s (s v51)) v1], [])
                                      (fst3 $
                                       split [50..] [maxo1 v15 (s (s v20)) v1] [maxo1 v50 (s (s v51)) v1])
  where
    checkVariant (x, x') (y, y') = isVariant x y && isVariant x' y'

    x = V 0
    y = V 1
    z = V 2
    f x y = Invoke "f" [x, y]
    g x = Invoke "g" [x]
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    leo x y = Invoke "leo" [x, y, trueo]
    lengtho x y = Invoke "lengtho" [x, y]
    s x = C "S" [x]
    v1  = V 1
    v15 = V 15
    v20 = V 20
    v33 = V 33
    v50 = V 50
    v51 = V 51
    v52 = V 52
    v53 = V 53
    v121 = V 121
    v122 = V 122
    v126 = V 126
    v127 = V 127
    v128 = V 128
    v150 = V 150
    v151 = V 151
    v152 = V 152
    v153 = V 153
    v154 = V 154

unit_abstract = do
  test (\goal -> map fst $ fst $ GC.abstract (Descend goal []  ) goal [11..]) goal [goal]
  where
    goal = [maxo1 v3 zero v1]
    maxo1 x y z = Invoke "maxo1" [x, y, z]
    v3 = V 3
    v1 = V 1
    zero = C "O" []

sa1 = Invoke "a" [V 1, C "c" [V 4, C "c" [V 5, C "n" []]] ,
                       C "c" [V 4, C "c" [V 6, C "n" []]]]

dsa1 = [Invoke "a" [V 5, V 1, V 6], sa1]

sa2 = Invoke "a" [V 1, C "c" [V 4, C "c" [V 7, C "c" [V 8, C "n" []]]] ,
                       C "c" [V 4, C "c" [V 7, C "c" [V 9, C "n" []]]]]

dsa2 = [Invoke "a" [V 8, V 1, V 9 ], sa2]


unit_littleInstTest = do
    test2 isInst ( ca (pair v73 zero) v45 (pair v73 zero) v2 true )
                 ( ca (pair v28 zero) v4  (pair v28 v43 ) v2 true )
                 False
    test2 isInst ( ca (pair v28 zero) v4  (pair v28 v43 ) v2 true )
                 ( ca (pair v73 zero) v45 (pair v73 zero) v2 true )
                 True
  where
    ca x y z m n = Invoke "checkAnswer'" [x, y, z, m, n]
    pair x y = C "pair" [x, y]
    true = C "true" []
    zero = C "zero" []
    v2 = V 2
    v4 = V 4
    v28 = V 28
    v43 = V 43
    v45 = V 45
    v73 = V 73


unit_littleTest = do
    manyAssert True  embed  [ ( [getAnswer' [v4,   st [false, true, false, true, false], some [v8]],  add [v10,v8, s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                              , [getAnswer' [v140, st [false, true, false, true, false], some [v144]],add [v10,v77,s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                              )
                            ]
    manyAssert True  embed  [ ( [getAnswer' [v4,   st [false, true, false, true, false], some [v8]],  add [v10,v8,s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                              , [getAnswer' [v140, st [false, true, false, true, false], some [v144]],add [v146,v144,v145], add [v78,v145,v77],add [v10,v77,s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(o)))))))))))))))))]]
                              )
                            ]
  where
    v4 = V 4
    v8 = V 8
    v9 = V 9
    v10 = V 10
    v77 = V 77
    v78 = V 78
    v140 = V 140
    v144 = V 144
    v145 = V 145
    v146 = V 146
    st = C "st"
    some = C "some"
    false = C "false" []
    true = C "true" []
    s x = C "s" [x]
    o = C "o" []
    getAnswer' x = Invoke "ga" x
    add x = Invoke "a" x

unit_isGroundTerm = do
  test isGroundTerm (V "") False
  test isGroundTerm (C "" [V ""]) False
  test isGroundTerm ((C "" [C "" [V ""], C "" [C "" [V ""]]])) False
  test isGroundTerm ((C "" [C "" [V ""], C "" [C "" []]])) False
  test isGroundTerm ((C "" [C "" [], C "" [C "" [V ""]]])) False

  test isGroundTerm (C "" []) True
  test isGroundTerm (C "a" [C "b" []]) True
  test isGroundTerm ((C "" [C "" [], C "" [C "" []]])) True

unit_getVars = do
  runTest [x] (v x)
  runTest [x] [v x, c [v x]]
  runTest [x, y] [v x, c [v x], d [v x, c [v y, v x]]]
  runTest [z] (f [c [v z, v z]])
  runTest [x, y, z] [f [c [v x], d [c [v y]]], g [v x, v z], g [c [v y], d [v z]]]
  where
    runTest expVars input =
      test getVars input (Set.fromList expVars)
    inv = Invoke
    f = inv "f"
    g = inv "g"
    c = C "c"
    d = C "d"
    v = V
    x = "x"
    y = "y"
    z = "z"

unit_generateFreshName = do
    runTest "f"   []                       "f"
    runTest "f"   ["g"]                    "f"
    runTest "f"   ["g", "_f", "abc"]       "f"
    runTest "_f"  ["_f"]                   "__f"
    runTest "f"   ["g", "_f", "f", "___f"] "__f"
    runTest "__f" ["g", "c", "abc"]        "__f"
  where
    runTest inputName names newName =
      let setNames = Set.fromList names in
      test (generateFreshName inputName) setNames newName


unit_renameGoals = do
    runTest [f [x], g [y]] [] "fG" [x, y]
    runTest [f [x], g [y]] ["f", "g"] "fG" [x, y]
    runTest [f [x], g [y]] ["fG"] "_fG" [x, y]
    runTest [f [x], g [y, y]] [] "fG" [x, y]
    runTest [f [x, y], g [y, y]] [] "fG" [x, y]
  where
    runTest goals names newName args =
      let setNames = map (\x -> ([], x, [])) names in
      let insertName name defs = (goals, name, args) : defs in
      test (renameGoals goals)
           setNames
           (insertName newName setNames, newName, args)
    inv n args = Invoke n $ map V args
    f = inv "f"
    g = inv "g"
    x = 0
    y = 1

unit_unifyInvocationsStuff = do
    runTest [] [] $ Just []
    runTest [f []] [f []] $ Just []
    runTest [f [x]] [f [c [y,z]]] $ Just [(0, c [y,z])]
    runTest [f [c [x],     y],     g [y,     c [d [x]]]]
            [f [c [c [z]], d [z]], g [d [z], c [d [c [z]]]]] $
            Just [(1, d [z]), (0, c [z])]
    runTest [f [x]] [f [c [x,x]]] $ Just [(0, c [x, x])]

    runTest [] [f []] Nothing
    runTest [f []] [g []] Nothing
    runTest [f []] [f [x]] Nothing
    -- test [f [x]] [f [c [x,x]]] Nothing
    -- test [f [c [x],     y], g [y,     c [d [x]]]]
    --      [f [c [c [z]], z], g [d [z], c [d [c [z]]]]]
    --      Nothing
  where
    runTest gs hs expected = test (unifyInvocationLists gs hs) (Just Subst.empty) (Map.fromList <$> expected)
    f = Invoke "f"
    g = Invoke "g"
    x = V 0
    y = V 1
    z = V 2
    c = C "c"
    d = C "d"
    e = C "e"

unit_ground = do
    test isGround ([] :: [G X]) True
    test isGround x False
    test isGround nil True
    test isGround (cons x nil) False
    test isGround (cons two nil) True
    test isGround (eq true true true) True
    test isGround (gt zero five false) True
    test isGround [eq true true true] True
    test isGround [checkPerson (st true true true true true) x true] False
    test isGround (maxim zero five x) False
    test isGround [ga x (st true true true true true) (some x), times b x, times d x, maxim x x x, add x x x, add x x five] False
  where
    x = V "x"
    y = V "y"
    b = C "b" []
    d = C "d" []
    true = C "true" []
    false = C "false" []
    nil = C "Nil" []
    cons h t = C "Cons" [h, t]
    some x = C "Some" [x]
    z = C "O" []
    s x = C "S" [x]
    two = s $ s z
    five = s . s . s . s . s $ z
    st x y z m n = C "st" [x, y, z, m, n]
    eq x y r = Invoke "eq" [x, y, r]
    gt x y r = Invoke "gt" [x, y, r]
    checkPerson x y r = Invoke "checkPerson" [x, y, r]
    maxim x y r = Invoke "max" [x, y, r]
    ga x y r = Invoke "getAnswer'" [x, y, r]
    times x y = Invoke "times" [x, y]
    add x y r = Invoke "add" [x, y, r]
