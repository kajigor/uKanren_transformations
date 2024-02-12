open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec addo y0 y1 = (fresh (q1) ((((y1 === (succ ((succ ((zero ())))))) &&& (y0 === (zero ()))) ||| ((y0 === (succ q1)) &&& (_addo y1 q1 ((succ ((zero ()))))))))) 
  and _addo y2 y3 y4 = (fresh (q1) ((((y3 === (zero ())) &&& (y2 === (succ ((succ y4))))) ||| ((y3 === (succ q1)) &&& (_addo y2 q1 ((succ y4))))))) 
  in   (addo x0 x2)
