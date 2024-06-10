open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec check y0 = (fresh (q1) ((((y0 === (((pair ((one ())) ((two ())))) % q1)) &&& (_check q1)) ||| ((y0 === (((pair ((one ())) ((thr ())))) % q1)) &&& (___check q1))))) 
  and _check y1 = (fresh (q1) (((y1 === (((pair ((two ())) ((thr ())))) % q1)) &&& (___check q1)))) 
  and ___check y2 = (fresh (q1) (((y2 === (((pair ((thr ())) ((two ())))) % q1)) &&& (_check q1)))) 
  in    (check x0)
