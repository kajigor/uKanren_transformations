module Programs where
import Data
import MiniKanren

appendo =
  Def "appendo" ["x", "y", "xy"]
    (Disj
       [ Conj (Unify (Var "x") (Ctor "Nil" []))
              (Unify (Var "xy") (Var "y"))
       , Fresh "h"
          (Fresh "t"
             (Fresh "ty"
                (Conj (Unify (Var "x") (Ctor "Cons" [Var "h", Var "t"]))
                      (Conj (Unify (Var "xy") (Ctor "Cons" [Var "h", Var "ty"]))
                            (Invoke "appendo" [Var "t", Var "y", Var "ty"])
                      )
                )
             )
          )
       ]
    )

doubleAppendo =
  Def "doubleAppendo" ["x", "y", "t", "z", "r"]
    (Conj (Invoke "appendo" [var "x", var "y", var "t"])
          (Invoke "appendo" [var "t", var "z", var "r"])
    )

reverso =
  Def "reverso" ["xs", "sx"]
    (Disj
      [ Conj (Unify (Var "xs") nil)
             (Unify (Var "sx") nil)
      , Fresh "h"
         (Fresh "t"
           (Conj (Unify (Var "xs") (Var "h" `cons` Var "t"))
                 (Fresh "tr"
                   (Conj (Invoke "reverso" [Var "t", Var "tr"])
                         (Invoke "appendo" [Var "tr", Var "h" `cons` nil, Var "sx"])
                   )
                 )
           )
         )
      ]
    )

revAcco =
  Def "revAcco" ["xs", "acc", "sx"] $
    conde [ [ var "xs" === nil, var "sx" === var "acc" ]
          , [ fresh ["h", "t"] $
                ( var "xs" === (var "h" `cons` var "t") )
                &&& Zzz (Invoke "revAcco" [var "t", var "h" `cons` var "acc", var "sx"])
            ]
          ]

revAcco' =
  Def "revAcco'" ["xs", "sx"] $ Invoke "revAcco" [var "xs", nil, var "sx"]

testSpec = Spec
  { defs = [ Def "A" [] (fresh ["x", "y"] $ Invoke "B" [var "x"] &&& Invoke "C" [var "y"] )
           , Def "B" ["x"] (fresh ["y"] $ Invoke "B" [var "y" `cons` var "x"])
           , Def "C" ["y"] (fresh ["z"] $ Invoke "C" [var "z" `cons` var "y"])
           ]
  , goal = Invoke "A" []
  }
