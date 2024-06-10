open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec sorto y0 = (fresh (q1 q2 q3) (((y0 === (q1 % ((q2 % ((q3 % ((List.nil ())))))))) &&& (minmaxoMinmaxo q1 q2 q3)))) 
  and minmaxoMinmaxo y1 y3 y4 = (((y1 === (zero ())) &&& (minmaxo y3 y4)) ||| ((y1 === (succ ((zero ())))) &&& (_minmaxo y3 y4))) 
  and minmaxo y5 y6 = ((y6 === (succ ((zero ())))) &&& (y5 === (succ ((zero ()))))) 
  and _minmaxo y7 y8 = (((y8 === (succ ((zero ())))) &&& (y7 === (zero ()))) ||| ((y8 === (zero ())) &&& (y7 === (succ ((zero ())))))) 
  in     (sorto x0)
