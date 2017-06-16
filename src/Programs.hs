module Programs where
import Data
import MiniKanren

appendo =
  Def "appendo" ["x", "y", "xy"]
    (Disj
       (Conj (Unify (Var "x") (Ctor "Nil" []))
             (Unify (Var "xy") (Var "y"))
       )
       (Fresh "h"
          (Fresh "t"
             (Fresh "ty"
                (Conj (Unify (Var "x") (Ctor "Cons" [Var "h", Var "t"]))
                      (Conj (Unify (Var "xy") (Ctor "Cons" [Var "h", Var "ty"]))
                            (Invoke "appendo" [Var "t", Var "y", Var "ty"])
                      )
                )
             )
          )
       )
    )

reverso =
  Def "reverso" ["xs", "sx"]
    (Disj
      (Conj (Unify (Var "xs") nil)
            (Unify (Var "sx") nil)
      )
      (Fresh "h"
        (Fresh "t"
          (Conj (Unify (Var "xs") (Var "h" `cons` Var "t"))
                (Fresh "tr"
                  (Conj (Invoke "reverso" [Var "t", Var "tr"])
                        (Invoke "appendo" [Var "tr", Var "h" `cons` nil, Var "sx"])
                  )
                )
          )
        )
      )
    )

--revAcco xs acc sx =
--  fun "revAcco" $
--    conde [ [ xs === nil, sx === acc]
--          , [ callFresh (\h ->
--                callFresh (\t ->
--                  xs === pair h t &&&
----                  callFresh (\y ->
----                    y === pair h acc &&&
----                    call (revAcco t y sx) [t, y, sx]
----                  )
--                  (let y = pair h acc in call (revAcco t y sx) [t, y, sx])
--                )
--              )
--            ]
--          ]
