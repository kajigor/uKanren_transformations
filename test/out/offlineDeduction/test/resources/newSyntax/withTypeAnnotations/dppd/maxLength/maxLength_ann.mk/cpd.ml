open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec max_length y0 y1 = ((y1 === (succ ((succ ((succ ((succ ((succ ((zero ())))))))))))) &&& (y0 === (succ ((succ ((zero ()))))))) 
  in  (max_length x0 x1)
