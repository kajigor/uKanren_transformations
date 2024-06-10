open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 x4 = 
  let rec generate y0 y1 y2 y3 y4 = (fresh (q1) (((y4 === (List.nil ())) ||| ((y4 === (y0 % ((y2 % q1)))) &&& (generate y0 y1 y2 y3 q1)) ||| ((y4 === (y0 % ((y3 % q1)))) &&& (generate y0 y1 y2 y3 q1)) ||| ((y4 === (y1 % ((y2 % q1)))) &&& (generate y0 y1 y2 y3 q1)) ||| ((y4 === (y1 % ((y3 % q1)))) &&& (generate y0 y1 y2 y3 q1))))) 
  in  (generate x0 x1 x2 x3 x4)
