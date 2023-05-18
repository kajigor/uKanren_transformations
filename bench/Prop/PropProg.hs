module PropProg where
import Syntax
import Def

andoDef = Def {
    getName = "ando", 
    getArgs = ["x","y","b"], 
    getBody = unsafeDisj [ 
        unsafeConj [V "x" :=: C "Trueo" [], V "y" :=: C "Trueo" [], V "b" :=: C "Trueo" []],
        unsafeConj [V "x" :=: C "Falso" [], V "y" :=: C "Trueo" [], V "b" :=: C "Falso" []],
        unsafeConj [V "x" :=: C "Trueo" [], V "y" :=: C "Falso" [], V "b" :=: C "Falso" []],
        unsafeConj [V "x" :=: C "Falso" [], V "y" :=: C "Falso" [], V "b" :=: C "Falso" []]
        ]
    }

oroDef = Def {
    getName = "oro", 
    getArgs = ["x","y","b"], 
    getBody = unsafeDisj [
        unsafeConj [V "x" :=: C "Trueo" [], V "y" :=: C "Trueo" [], V "b" :=: C "Trueo" []], 
        unsafeConj [V "x" :=: C "Falso" [], V "y" :=: C "Trueo" [], V "b" :=: C "Trueo" []], 
        unsafeConj [V "x" :=: C "Trueo" [], V "y" :=: C "Falso" [], V "b" :=: C "Trueo" []],
        unsafeConj [V "x" :=: C "Falso" [], V "y" :=: C "Falso" [], V "b" :=: C "Falso" []]
        ]
    }
notoDef = Def {
    getName = "noto",
    getArgs = ["x","b"], 
    getBody = unsafeDisj [
        unsafeConj [V "x" :=: C "Trueo" [], V "b" :=: C "Falso" []], 
        unsafeConj [V "x" :=: C "Falso" [], V "b" :=: C "Trueo" []]]

    }

evaloDef = Def {
    getName = "evalo", 
    getArgs = ["st","fm","u"], 
    getBody = Fresh "x" (Fresh "y" (Fresh "v" (Fresh "w" (Fresh "z" (
        unsafeDisj [
            unsafeConj [
                Invoke "ando" [V "v",V "w",V "u"], 
                Delay $ Invoke "evalo" [V "st",V "x",V "v"], 
                Delay $ Invoke "evalo" [V "st",V "y",V "w"],
                V "fm" :=: C "Conj" [V "x",V "y"]
            ], 
            unsafeConj [
                Invoke "oro" [V "v",V "w",V "u"], 
                Delay $ Invoke "evalo" [V "st",V "x",V "v"], 
                Delay $ Invoke "evalo" [V "st",V "y",V "w"],
                V "fm" :=: C "Disj" [V "x",V "y"]
            ], 
            unsafeConj [
                Invoke "noto" [V "v",V "u"], 
                Delay $ Invoke "evalo" [V "st",V "x",V "v"], 
                V "fm" :=: C "Neg" [V "x"]
            ],
            unsafeConj [
                V "fm" :=: C "Var" [V "z"], 
                Invoke "elemo" [V "z",V "st",V "u"]
            ]
        ]
        )))))
    }

elemoDef = Def {
    getName = "elemo", 
    getArgs = ["n","s","v"], 
    getBody = Fresh "h" (Fresh "t" (Fresh "n'" (
        unsafeDisj [
            unsafeConj [V "n" :=: C "Zero" [], V "s" :=: C "Cons" [V "h",V "t"], V "v" :=: V "h"], 
            unsafeConj [V "s" :=: C "Cons" [V "h",V "t"], Invoke "elemo" [V "n'",V "t",V "v"], V "n" :=: C "Succ" [V "n'"]]
        ]
        )))
    }

evalo = [andoDef, oroDef, notoDef, evaloDef, elemoDef]