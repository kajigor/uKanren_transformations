open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec evalo y0 y1 = (fresh (q1) ((((y1 === (succ ((succ ((zero ())))))) &&& (y0 === (zero ()))) ||| ((y0 === (succ q1)) &&& (addoAddo y1 q1))))) 
  and addoAddo y3 y4 = (fresh (q1) ((((y4 === (zero ())) &&& (y3 === (succ ((zero ()))))) ||| ((y4 === (succ q1)) &&& (addoAddo ((succ y3)) q1))))) 
  in   (evalo x0 x1)
