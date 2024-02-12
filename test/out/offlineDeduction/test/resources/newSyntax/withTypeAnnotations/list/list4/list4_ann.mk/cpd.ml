open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec maxMino y0 y1 = ((maxo1 y0) &&& (mino1 y1)) 
  and maxo1 y2 = (_maxo1 y2) 
  and _maxo1 y3 = (y3 === (succ ((succ ((zero ())))))) 
  and mino1 y4 = (_mino1 y4) 
  and _mino1 y5 = (y5 === (zero ())) 
  in      (maxMino x1 x2)
