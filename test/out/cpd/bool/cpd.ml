open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec noto y0 y1 = (((y1 === (trueo ())) &&& (y0 === (falso ()))) ||| ((y1 === (falso ())) &&& (y0 === (trueo ())))) 
  in  (noto x0 x1)
