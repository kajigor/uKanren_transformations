module Bool where 

import Syntax

falso = C "false" []
trueo = C "true"  []

nando g = 
  let a = V "a" in 
  let b = V "b" in 
  let c = V "c" in 
  Let 
    (
      def "nando" ["a", "b", "c"]
        ( 
          ( a === falso &&& b === falso &&& c === trueo ) |||
          ( a === falso &&& b === trueo &&& c === trueo ) |||
          ( a === trueo &&& b === falso &&& c === trueo ) |||
          ( a === trueo &&& b === trueo &&& c === falso )
        )
    ) g

noto g = 
  let a = V "a" in  
  let na = V "na" in 
  Let ( def "noto" ["a", "na"] ( call "nando" [a, a, na] ) ) (nando g) 

oro g = 
  let a = V "a" in 
  let b = V "b" in 
  let c = V "c" in 
  let aa = V "aa" in 
  let bb = V "bb" in
  Let 
    (
      def "oro" ["a", "b", "c"] 
        (
          fresh ["aa", "bb"]
            (
              call "nando" [a, a, aa] &&& 
              call "nando" [b, b, bb] &&&
              call "nando" [aa, bb, c]
            )
        ) 
    ) (nando g)

ando g =
  let a = V "a" in 
  let b = V "b" in 
  let c = V "c" in 
  let ab = V "ab" in 
  Let (
    def "ando" ["a", "b", "c"] (
      fresh ["ab"] (
        call "nando" [a, b, ab] &&&
        call "nando" [ab, ab, c]
      )
    )
  ) (nando g)

bool (C "false" []) = "false"
bool (C "true"  []) = "true"

