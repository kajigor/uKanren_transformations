open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec evalo y0 = (fresh (q1 q2) (((y0 === (lit !!true)) ||| ((y0 === (disj q1 q2)) &&& (evalo q1) &&& (evalo q2)) ||| ((y0 === (disj q1 q2)) &&& (_evalo q1) &&& (evalo q2)) ||| ((y0 === (disj q1 q2)) &&& (evalo q1) &&& (_evalo q2)) ||| ((y0 === (conj q1 q2)) &&& (evalo q1) &&& (evalo q2))))) 
  and _evalo y1 = (fresh (q1 q2) (((y1 === (lit !!false)) ||| ((y1 === (disj q1 q2)) &&& (_evalo q1) &&& (_evalo q2)) ||| ((y1 === (conj q1 q2)) &&& (_evalo q1) &&& (evalo q2)) ||| ((y1 === (conj q1 q2)) &&& (evalo q1) &&& (_evalo q2)) ||| ((y1 === (conj q1 q2)) &&& (_evalo q1) &&& (_evalo q2))))) 
  in   (evalo x0)
