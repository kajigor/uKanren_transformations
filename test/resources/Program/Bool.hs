module Program.Bool where

import Syntax

falso :: Term a
falso = C "false" []

trueo :: Term a
trueo = C "true"  []

nando :: G a -> G a
nando g =
  Let
    ( Def "nando" ["a", "b", "c"]
        (
          ( a === falso &&& b === falso &&& c === trueo ) |||
          ( a === falso &&& b === trueo &&& c === trueo ) |||
          ( a === trueo &&& b === falso &&& c === trueo ) |||
          ( a === trueo &&& b === trueo &&& c === falso )
        )
    ) g
    where [a, b, c] = map V ["a", "b", "c"]

noto :: G a -> G a
noto g =
  Let 
    ( Def "noto" ["a", "na"] 
      ( 
        call "nando" [a, a, na] 
      ) 
    ) $ nando g
    where [a, na] = map V ["a", "na"]

oro :: G a -> G a
oro g =
  Let
    ( Def "oro" ["a", "b", "c"]
        (
          fresh ["aa", "bb"]
            (
              call "nando" [a, a, aa] &&&
              call "nando" [b, b, bb] &&&
              call "nando" [aa, bb, c]
            )
        )
    ) $ nando g
    where [a, b, c, aa, bb] = map V ["a", "b", "c", "aa", "bb"]

ando :: G a -> G a
ando g =
  Let 
    ( Def "ando" ["a", "b", "c"] 
      (
        fresh ["ab"] 
        (
          call "nando" [a, b, ab] &&&
          call "nando" [ab, ab, c]
        )
      )
    ) $ nando g
    where [a, b, c, ab] = map V ["a", "b", "c", "ab"]

bool :: Term t -> String
bool (C "false" []) = "false"
bool (C "true"  []) = "true"
bool _ = "?!"