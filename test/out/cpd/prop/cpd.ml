open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec evalo y0 = ((___evaloEvalo y0 ((trueo ()))) ||| (__evaloEvalo y0 ((trueo ()))) ||| (___evaloEvalo y0 ((falso ())))) 
  and andoEvaloEvaloEvaloEvalo y5 y6 y7 y8 = (((__evaloEvalo y5 y7) &&& (___evaloEvalo y6 y8)) ||| ((___evaloEvalo y5 y7) &&& (__evaloEvalo y6 y8)) ||| ((__evaloEvalo y5 y7) &&& (__evaloEvalo y6 y8))) 
  and __evaloEvalo y9 y10 = (fresh (q1 q2 q3 q4) ((((y10 === (falso ())) &&& (y9 === (lit ((falso ()))))) ||| ((y9 === (neg q1)) &&& (___evaloEvalo q1 q2) &&& (noto y10 q2)) ||| ((y9 === (disj q1 q3)) &&& (__evaloEvalo q1 q2) &&& (__evaloEvalo q3 q4) &&& (oro y10 q2 q4)) ||| ((y9 === (conj q1 q3)) &&& (andoEvaloEvaloEvaloEvalo q1 q3 q2 q4) &&& (ando y10 q2 q4)) ||| ((y9 === (impl q1 q3)) &&& (___evaloEvalo q1 q2) &&& (__evaloEvalo q3 q4) &&& (implicationo y10 q2 q4))))) 
  and noto y11 y12 = (((y12 === (trueo ())) &&& (y11 === (falso ()))) ||| ((y12 === (falso ())) &&& (y11 === (trueo ())))) 
  and oro y13 y14 y15 = (((y15 === (trueo ())) &&& (y14 === (trueo ())) &&& (y13 === (trueo ()))) ||| ((y15 === (trueo ())) &&& (y14 === (falso ())) &&& (y13 === (trueo ()))) ||| ((y15 === (falso ())) &&& (y14 === (trueo ())) &&& (y13 === (trueo ()))) ||| ((y15 === (falso ())) &&& (y14 === (falso ())) &&& (y13 === (falso ())))) 
  and ando y16 y17 y18 = (((y18 === (trueo ())) &&& (y17 === (trueo ())) &&& (y16 === (trueo ()))) ||| ((y18 === (trueo ())) &&& (y17 === (falso ())) &&& (y16 === (falso ()))) ||| ((y18 === (falso ())) &&& (y17 === (trueo ())) &&& (y16 === (falso ()))) ||| ((y18 === (falso ())) &&& (y17 === (falso ())) &&& (y16 === (falso ())))) 
  and implicationo y19 y20 y21 = (((y21 === (trueo ())) &&& (y20 === (falso ())) &&& (y19 === (trueo ()))) ||| ((y21 === (falso ())) &&& (y20 === (falso ())) &&& (y19 === (trueo ()))) ||| ((y21 === (trueo ())) &&& (y20 === (trueo ())) &&& (y19 === (trueo ()))) ||| ((y21 === (falso ())) &&& (y20 === (trueo ())) &&& (y19 === (falso ())))) 
  and ___evaloEvalo y22 y23 = (fresh (q1 q2 q3 q4) ((((y23 === (trueo ())) &&& (y22 === (lit ((trueo ()))))) ||| ((y22 === (neg q1)) &&& (__evaloEvalo q1 q2) &&& (noto y23 q2)) ||| ((y22 === (disj q1 q3)) &&& (oroEvaloEvaloEvaloEvalo q1 q3 q2 q4) &&& (oro y23 q2 q4)) ||| ((y22 === (conj q1 q3)) &&& (___evaloEvalo q1 q2) &&& (___evaloEvalo q3 q4) &&& (ando y23 q2 q4)) ||| ((y22 === (impl q1 q3)) &&& (implicationoEvaloEvaloEvaloEvalo q1 q3 q2 q4) &&& (implicationo y23 q2 q4))))) 
  and oroEvaloEvaloEvaloEvalo y26 y27 y28 y29 = (((___evaloEvalo y26 y28) &&& (___evaloEvalo y27 y29)) ||| ((__evaloEvalo y26 y28) &&& (___evaloEvalo y27 y29)) ||| ((___evaloEvalo y26 y28) &&& (__evaloEvalo y27 y29))) 
  and implicationoEvaloEvaloEvaloEvalo y32 y33 y34 y35 = (((__evaloEvalo y32 y34) &&& (___evaloEvalo y33 y35)) ||| ((__evaloEvalo y32 y34) &&& (__evaloEvalo y33 y35)) ||| ((___evaloEvalo y32 y34) &&& (___evaloEvalo y33 y35))) 
  in           (evalo x0)
