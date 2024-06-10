open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec help y0 y1 y2 = ((y2 === (((succ ((zero ())))) % ((((succ ((zero ())))) % ((((zero ())) % ((((succ ((succ ((zero ())))))) % ((List.nil ())))))))))) &&& (y1 === (succ ((zero ())))) &&& (y0 === (zero ()))) 
  in  (help x0 x1 x2)
