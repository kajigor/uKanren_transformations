open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec solve y0 y1 y2 = (fresh (q1) (((y0 === (term ((null ())) ((List.nil ())))) &&& (mkng y1 y2)))) 
  and mkng y3 y4 = (y3 === y4) 
  in   (solve x0 x1 x2)
