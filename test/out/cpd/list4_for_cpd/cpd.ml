open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec maxMino y0 y1 = ((y1 === (zero ())) &&& (y0 === (succ ((succ ((zero ()))))))) 
  in  (maxMino x0 x1)
