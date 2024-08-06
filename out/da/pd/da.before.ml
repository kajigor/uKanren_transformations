open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = 
  let rec doubleAppendo y0 y1 y2 y3 = (fresh (x4 x5 x7 x6) ((success &&& success &&& (success &&& (((y1 === x4) &&& (y0 === (List.nil ()))) ||| ((x4 === (x5 % x7)) &&& (y0 === (x5 % x6)) &&& (appendo x6 y1 x7)))) &&& (success &&& (appendo x4 y2 y3))))) 
  and appendo y4 y5 y6 = (fresh (x5 x7 x6) ((success &&& (((y5 === y6) &&& (y4 === (List.nil ()))) ||| ((y6 === (x5 % x7)) &&& (y4 === (x5 % x6)) &&& (appendo x6 y5 x7)))))) 
  in   (doubleAppendo x0 x1 x2 x3)
