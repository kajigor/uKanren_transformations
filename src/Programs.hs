module Programs where
import MuKanren

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
