open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec evalo y0 = (fresh (q1 q2) (((y0 === (lit !!true)) ||| ((y0 === (disj q1 q2)) &&& (oroEvaloEvalo q1 q2)) ||| ((y0 === (conj q1 q2)) &&& (evalo q1) &&& (evalo q2))))) 
  and oroEvaloEvalo y1 y2 = (((evalo y1) &&& (evalo y2)) ||| ((_evalo y1) &&& (evalo y2)) ||| ((evalo y1) &&& (_evalo y2))) 
  and _evalo y5 = (fresh (q1 q2) (((y5 === (lit !!false)) ||| ((y5 === (disj q1 q2)) &&& (_evalo q1) &&& (_evalo q2)) ||| ((y5 === (conj q1 q2)) &&& (andoEvaloEvalo q1 q2))))) 
  and andoEvaloEvalo y6 y7 = (((_evalo y6) &&& (evalo y7)) ||| ((evalo y6) &&& (_evalo y7)) ||| ((_evalo y6) &&& (_evalo y7))) 
  in     (evalo x0)
