open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec max_length y0 y1 = ((y1 === (succ ((succ ((succ ((succ ((succ ((zero ())))))))))))) &&& (max1 y0)) 
  and max1 y2 = (_max1 y2) 
  and _max1 y3 = (__max1 y3) 
  and __max1 y4 = (___max1 y4) 
  and ___max1 y5 = (y5 === (succ ((succ ((zero ())))))) 
  in      (max_length x0 x1)
