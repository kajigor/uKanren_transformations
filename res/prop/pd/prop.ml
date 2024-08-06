open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec evalo y0 = (fresh (q1 q2 q3 q4) (((y0 === (lit !!true)) ||| ((y0 === (disj q1 q2)) &&& (((q3 === !!true) &&& (q4 === !!true)) ||| ((q3 === !!true) &&& (q4 === !!false)) ||| ((q3 === !!false) &&& (q4 === !!true))) &&& (_evalo q1 q4) &&& (_evalo q2 q3)) ||| ((y0 === (conj q1 q2)) &&& (_evalo q1 !!true) &&& (_evalo q2 !!true))))) 
  and _evalo y1 y2 = (fresh (q1 q2 q3 q4) (((y1 === (lit y2)) ||| ((y1 === (disj q1 q2)) &&& (((q3 === !!true) &&& (q4 === !!true) &&& (y2 === !!true)) ||| ((q3 === !!true) &&& (q4 === !!false) &&& (y2 === !!true)) ||| ((q3 === !!false) &&& (q4 === !!true) &&& (y2 === !!true)) ||| ((q3 === !!false) &&& (q4 === !!false) &&& (y2 === !!false))) &&& (_evalo q1 q4) &&& (_evalo q2 q3)) ||| ((y1 === (conj q1 q2)) &&& (((q3 === !!true) &&& (q4 === !!true) &&& (y2 === !!true)) ||| ((q3 === !!true) &&& (q4 === !!false) &&& (y2 === !!false)) ||| ((q3 === !!false) &&& (q4 === !!true) &&& (y2 === !!false)) ||| ((q3 === !!false) &&& (q4 === !!false) &&& (y2 === !!false))) &&& (_evalo q1 q4) &&& (_evalo q2 q3))))) 
  in   (evalo x0)
