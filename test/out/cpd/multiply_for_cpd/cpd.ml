open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec multiply y0 y1 = (fresh (q1 q2) ((((y1 === (succ q1)) &&& (y0 === (zero ())) &&& (multiply ((zero ())) q1)) ||| ((y1 === (succ q1)) &&& (y0 === (succ q2)) &&& (addMultiply q1 q2))))) 
  and addMultiply y2 y4 = (fresh (q1 q2) ((((y4 === (zero ())) &&& (y2 === (succ ((succ ((succ q1)))))) &&& (_multiply q1)) ||| ((y4 === (succ q2)) &&& (_addMultiply y2 q2))))) 
  and _multiply y5 = (y5 === (zero ())) 
  and _addMultiply y6 y8 = (fresh (q1 q2) ((((y8 === (zero ())) &&& (y6 === (succ q1)) &&& (__multiply q1)) ||| ((y8 === (succ q2)) &&& (__addMultiply y6 q2))))) 
  and __multiply y9 = (y9 === (zero ())) 
  and __addMultiply y10 y12 = ((y12 === (succ ((zero ())))) &&& (___multiply y10)) 
  and ___multiply y13 = (y13 === (zero ())) 
  in        (multiply x0 x1)
