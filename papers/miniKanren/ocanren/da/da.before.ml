open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = let rec doubleAppendo y0 y1 y2 y3 = (fresh (x4) ((success &&& (success &&& ((success &&& (appendo y0 y1 x4)) &&& (success &&& (appendo x4 y2 y3))))))) and appendo y4 y5 y6 = (fresh (x7 x6 x5) ((success &&& (((y4 === (Std.List.nil ())) &&& (y5 === y6)) ||| (((y4 === (Std.(%) (x5) (x6))) &&& (y6 === (Std.(%) (x5) (x7)))) &&& (appendo x6 y5 x7)))))) in   (doubleAppendo x0 x1 x2 x3)
