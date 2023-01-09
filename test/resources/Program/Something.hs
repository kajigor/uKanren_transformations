module Program.Something where

import Prelude hiding (sum)

import Syntax
import Def

add :: [Def G X]
add = [addDef]

addDef :: Def G X
addDef =
    Def "add" ["a", "b", "q14"] (
      ((V "a" === C "o" []) &&&
      (V "b" === V "q14")) |||
      (fresh ["x"] (
        (V "a" === C "s" [V "x"]) &&&
        (fresh ["q16"] (
            (V "q14" === C "s" [V "q16"]) &&&
            (call "add" [V "x", V "b", V "q16"])))))
    )

value :: [Def G X]
value = [valueDef]

valueDef :: Def G X
valueDef =
    Def "value" ["e", "q10"] (
      ((V "e" === C "q2" []) &&&
      (V "q10" === C "some" [C "s" [C "s" [C "o" []]]])) |||
      ((V "e" === C "q5" []) &&&
      (V "q10" === C "some" [C "s" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]]]])) |||
      ((V "e" === C "err" []) &&&
      (V "q10" === C "none" []))
    )

sum :: [Def G X]
sum = sumDef : value ++ add

sumDef :: Def G X
sumDef =
    Def "sum" ["l", "q0"] (
      ((V "l" === C "nil" []) &&&
      (V "q0" === C "some" [C "o" []])) |||
      (fresh ["x", "xs"] (
        (V "l" === C "%" [V "x", V "xs"]) &&&
        (fresh ["q3"] (
            (call "value" [V "x", V "q3"]) &&&
            (((V "q3" === C "none" []) &&&
            (V "q0" === C "none" [])) |||
            (fresh ["v"] (
              (V "q3" === C "some" [V "v"]) &&&
              (fresh ["q6"] (
                  (call "sum" [V "xs", V "q6"]) &&&
                  (((V "q6" === C "none" []) &&&
                  (V "q0" === C "none" [])) |||
                  (fresh ["v'"] (
                    (V "q6" === C "some" [V "v'"]) &&&
                    (fresh ["q8"] (
                        (V "q0" === C "some" [V "q8"]) &&&
                        (call "add" [V "v", V "v'", V "q8"])))))))))))))))
    )

tree = sum ++ add ++ value