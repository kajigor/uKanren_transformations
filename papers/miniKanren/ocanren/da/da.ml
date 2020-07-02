open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = let rec doubleAppendo y0 y1 y2 y3 = (fresh (q1) (((appendo y0 y1 q1) &&& (appendo q1 y2 y3)))) and appendo y4 y5 y6 = (fresh (q1 q2 q3) ((((y4 === (Std.List.nil ())) &&& (y5 === y6)) ||| (((y4 === (Std.(%) (q1) (q2))) &&& (y6 === (Std.(%) (q1) (q3)))) &&& (appendo q2 y5 q3))))) in   (doubleAppendo x0 x1 x2 x3)
