open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec reverso y0 y1 = (fresh (x2 x3 x4 x5 x6 x7) ((success &&& (((y1 === (List.nil ())) &&& (y0 === (List.nil ()))) ||| ((y0 === (x2 % x3)) &&& (success &&& (_reverso x3 x4)) &&& (success &&& (((x4 === (List.nil ())) &&& (y1 === (x2 % ((List.nil ()))))) ||| ((x4 === (x5 % x6)) &&& (y1 === (x5 % x7)) &&& (appendo x7 x2 x6))))))))) 
  and _reverso y2 y3 = (fresh (x2 x3 x4 x5 x6 x7) ((success &&& (((y3 === (List.nil ())) &&& (y2 === (List.nil ()))) ||| ((y2 === (x2 % x3)) &&& (success &&& (_reverso x3 x4)) &&& (success &&& (((x4 === (List.nil ())) &&& (y3 === (x2 % ((List.nil ()))))) ||| ((x4 === (x5 % x6)) &&& (y3 === (x5 % x7)) &&& (appendo x7 x2 x6))))))))) 
  and appendo y4 y5 y6 = (fresh (x0 x3 x5 x6 x7) (((x0 === (y5 % x3)) &&& (((y6 === (List.nil ())) &&& (y4 === (y5 % ((List.nil ()))))) ||| ((y6 === (x5 % x6)) &&& (y4 === (x5 % x7)) &&& (appendo x7 y5 x6)))))) 
  in    (_reverso x0 x1)
