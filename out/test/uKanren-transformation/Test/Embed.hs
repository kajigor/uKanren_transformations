module Test.Embed where

import           Test.Helper (manyAssert, test)

import           Embed
import           Syntax

unit_homeo = do
  testHomeoTerm
  testHomeoGoal
  testHomeoConj
    where
      testHomeoTerm = do
        manyAssert True  homeo [ (x, y)
                               , (v, c [v])
                               , (n [v], n [m [v]])
                               ]
        manyAssert False homeo [ (c [], v)
                               , (v, c [])
                               , (c [v], n [m [v]])
                               ]
      testHomeoGoal = do
        manyAssert True  homeo [ (f [], f [])
                               , (f [v], f [x])
                               , (f [n [v]], f [m [n [v]]])
                               , (f [m [v], n [v, v]], f [m [x], n [v, m [v]]])
                               ]
        manyAssert False homeo [ (f [], g [])
                               , (f [c [v]], f [n [m [v]]])
                               , (f [m [v], n [v, m []]], f [m [], n [v, v]])
                               ]
      testHomeoConj = do
        manyAssert True  homeo [ ([f []], [f []])
                               , ([f [], g[]], [f [], f [], g []])
                               , ([f [v]], [f [x]])
                               , ([f [n [v]]], [f [m [n [v]]]])
                               , ([f [m [v], n [v, v]]], [f [m [x], n [v, m [v]]]])
                               ]
        manyAssert False homeo [ ([f []], [g []])
                               , ([f [c [v]]], [f [n [m [v]]]])
                               ]

unit_inst = do
  testInstTerm
  testInstGoal
  testInstConj
    where
      testInstTerm = do
        manyAssert True  isInst [ (v, x)
                                , (v, n [])
                                , (n [], n [])
                                , (n [x, y], n [y, x])
                                , (n [x, y], n [x, x])
                                , (n [x, m [x]], n [y, m [y]])
                                ]

        manyAssert False isInst [ (n [], v)
                                , (n [v], n [])
                                , (n [x, x], n [y, x])
                                , (n [x, m [x]], n [x, y])
                                , (n [x, m [x]], n [x, m [y]])
                                ]

      testInstGoal = do
        manyAssert True  isInst [ (f [v], f [n [x]])
                                , (f [x, y], f [y, x])
                                , (f [x, y], f [x, x])
                                ]

        manyAssert False isInst [ (f [], g [])
                                , (f [n [x, m [x]]], f [n [x, m [y]]])
                                , (f [], f [v])
                                , (f [v], f [])
                                ]
      testInstConj = do
        manyAssert True  isInst [ ([f [v]], [f [n [x]]])
                                , ([f [x, y], g [v]], [f [x, x], g [x]])
                                , ([f [x, y], g [y]], [f [x, x], g [x]])
                                ]

        manyAssert False isInst [ ([], [f []])
                                , ([f [n [x, m [x]]]], [f [n [x, m [y]]]])
                                , ([f [x, y], g [x]], [f [x, x], g [y]])
                                ]

unit_strictInst = do
  testStrictInstTerm
  testStrictInstGoal
  testStrictInstConj
    where
      testStrictInstTerm = do
        manyAssert True  isStrictInst [ (v, n [])
                                      , (n [x, y], n [x, x])
                                      ]

        manyAssert False isStrictInst [ (v, x)
                                      , (n [], v)
                                      , (n [], n [])
                                      , (n [v], n [])
                                      , (n [x, y], n [y, x])
                                      , (n [x, x], n [y, x])
                                      , (n [x, m [x]], n [y, m [y]])
                                      , (n [x, m [x]], n [x, y])
                                      , (n [x, m [x]], n [x, m [y]])
                                      ]

      testStrictInstGoal = do
        manyAssert True  isStrictInst [ (f [v], f [n [x]])
                                      , (f [x, y], f [x, x])
                                      ]

        manyAssert False isStrictInst [ (f [], g [])
                                      , (f [n [x, m [x]]], f [n [x, m [y]]])
                                      , (f [], f [v])
                                      , (f [v], f [])
                                      , (f [x, y], f [y, x])
                                      ]
      testStrictInstConj = do
        manyAssert True isStrictInst  [ ([f [v]], [f [n [x]]])
                                      , ([f [x, y], g [v]], [f [x, x], g [x]])
                                      , ([f [x, y], g [y]], [f [x, x], g [x]])
                                      , ([f [x, y]], [f [x, x]])
                                      ]

        manyAssert False isStrictInst [ ([], [f []])
                                      , ([f [n [x, m [x]]]], [f [n [x, m [y]]]])
                                      , ([f [x, y], g [x]], [f [x, x], g [y]])
                                      ]

unit_embed = do
  testEmbedGoal
  testEmbedConj
    where
      testEmbedGoal = do
        manyAssert False embed [ (f [n [x, v], x, y], f [v, x, y])
                              , (f [x, x], f [x, y])
                              , (f [x, x, y, y], f [x, x, y, z])
                              ]

        manyAssert True embed  [ (f [x, y], f [x, x])
                              , (f [x, x, y, y], f [x, z, z, x])
                              , (f [x, z, z, x], f [x, x, y, z])
                              , (f [v, x, x], f [n [x, v], x, y])
                              , (f [v, x, n [y, x]], f [v, x, n [y, x]])
                              , (f [x, y, x], f [x, y, y])
                              , (f [x, y, y], f [x, y, x])
                              , (f [v, x, y], f [x, y, v]) -- variant
                              , (f [v, x, y], f [n [x, v], x, y]) -- not a strict instance
                              , (f [c [], m [x, x]], f [n [c []], m [x, y]])
                              , (maxo1 v15 (s (s v20)) v1, maxo1 v50 (s (s v51)) v1)
                              ]

      testEmbedConj = do
        manyAssert False embed [ ([f [n [x, v], x, y]], [f [v, x, y]])
                              , ([f [x, x]], [f [x, y]])
                              , ([f [x, x, y, y]], [f [x, x, y, z]])
                              , ([f [x, y], g []], [f [x, x]])
                              , ([g [], f [x, y]], [f [x, x]])
                              , ([f [x, x], f [x, x]], [f [x, x]])
                              , ([f [x, y], f [x, y]], [f [x, x]])
                              ]

        manyAssert True embed  [ ([f [x, y]], [f [x, x]])
                              , ([f [x, y]], [g [x], f [x, x]])
                              , ([f [x, y]], [g [], f [x, x], g []])
                              , ([f [x, x, y, y]], [f [x, z, z, x]])
                              , ([f [x, z, z, x]], [f [x, x, y, z]])
                              , ([f [v, x, x]], [f [n [x, v], x, y]])
                              , ([f [x, x, y, y], f [x, z, z, x], f [v, x, x]], [f [x, z, z, x], g [], g[], f [x, x, y, z], f [n [x, v], x, y]])
                              , ([f [v, x, n [y, x]]], [f [v, x, n [y, x]]])
                              , ([f [x, y, x]], [f [x, y, y]])
                              , ([f [x, y, y]], [f [x, y, x]])
                              , ([f [v, x, y]], [f [x, y, v]]) -- variant
                              , ([f [v, x, y]], [f [n [x, v], x, y]]) -- not a strict instance
                              , ([f [c [], m [x, x]]], [f [n [c []], m [x, y]]])
                              ]

unit_variant = do -- TODO more  tests
  manyAssert False isVariant [ (f [x, y], f [x, x])
                             , (f [x, x], f [x, y])
                             ]
  manyAssert False isVariant [ (app v19 (cons v18 (cons v12 nil)) (cons v12 (cons v18 v19)),
                                 app v22 (cons v18 (cons v21 nil)) (cons v18 (cons v21 v22)))
                             ]
  manyAssert True  isVariant [ ([app v12' v1' v13, app v15' v2' v16]
                             , [app v6 v1' v7, app v9 v2' v10])
                             ]

unit_renaming = do -- TODO more tests
  manyAssert True  isRenaming [ (f [x, y], f [x, x])
                              , (f [x], f [y])
                              ]
  manyAssert False isRenaming [ (f [x, x], f [x, y])
                              , (f [x, x], g [x, x])
                              ]
  manyAssert True  isRenaming [ ([app v12' v1' v13, app v15' v2' v16]
                              ,  [app v18' v1' v19, app v21' v2' v22])
                              ]

unit_ground = do
  test isGround x False
  test isGround (n [x, y]) False
  test isGround (m [n [], n []]) True
  test isGround (app nil (cons (n []) nil) (cons (m [n []]) nil)) True
  test isGround (app nil (cons x nil) (cons (m [n []]) nil)) False
  test isGround (app nil (cons (n []) nil) (cons (m [x]) nil)) False

unit_subconjs = do
  test (flip subconjs 0) [1, 2, 3] [[]]
  test (flip subconjs 1) [1, 2, 3] [[1], [2], [3]]
  test (flip subconjs 2) [1, 2, 3] [[1, 2], [1, 3], [2, 3]]
  test (flip subconjs 3) ([] :: [Int]) []


x = V "x"
y = V "y"
z = V "z"
v = V ""
c = C ""
n = C "n"
m = C "m"
f = Invoke "f"
g = Invoke "g"
maxo1 x y z = Invoke "maxo1" [x, y, z]
v1 = V "1"
v15 = V "15"
v20 = V "20"
v50 = V "50"
v51 = V "51"
s x = C "s" [x]
app x y xy = Invoke "appendo" [x, y, xy]
v12 = V 12
v19 = V 19
v18 = V 18
v21 = V 21
v22 = V 22
nil = C "Nil" []
cons h t = C "Cons" [h, t]
v12' = V 12
v1' = V 1
v2' = V 2
v13 = V 13
v15' = V 15
v16 = V 16
v6 = V 6
v7 = V 7
v9 = V 9
v10 = V 10
v18' = V 18
v21' = V 21
