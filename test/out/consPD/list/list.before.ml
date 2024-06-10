open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 x4 = 
  let rec appendoAppendo y0 y1 y2 y3 y4 = (fresh (x8 x10 x9 x14 x16 x15 x7 x12 x5 x11 x13 x6) ((success &&& (((y3 === y4) &&& (y2 === (List.nil ())) &&& (y1 === y2) &&& (y0 === (List.nil ()))) ||| ((y4 === (x8 % x10)) &&& (y2 === (x8 % x9)) &&& (y1 === y2) &&& (y0 === (List.nil ())) &&& (((x9 === (List.nil ())) &&& (y3 === x10)) ||| ((x10 === (x14 % x16)) &&& (x9 === (x14 % x15)) &&& (appendo y3 x15 x16)))) ||| ((x7 === x12) &&& (x5 === x11) &&& (y4 === (x11 % x13)) &&& (y2 === (x5 % x7)) &&& (y0 === (x5 % x6)) &&& (_appendoAppendo x6 y1 x12 y3 x13)))))) 
  and appendo y5 y6 y7 = (fresh (x4 x8 x2 x1 x0 x14 x16 x15) (((x4 === (x8 % y7)) &&& (x2 === (x8 % y6)) &&& (x1 === x2) &&& (x0 === (List.nil ())) &&& (((y6 === (List.nil ())) &&& (y5 === y7)) ||| ((y7 === (x14 % x16)) &&& (y6 === (x14 % x15)) &&& (appendo y5 x15 x16)))))) 
  and _appendoAppendo y8 y9 y10 y11 y12 = (fresh (x8 x10 x9 x14 x16 x15 x7 x12 x5 x11 x13 x6) ((success &&& (((y11 === y12) &&& (y10 === (List.nil ())) &&& (y9 === y10) &&& (y8 === (List.nil ()))) ||| ((y12 === (x8 % x10)) &&& (y10 === (x8 % x9)) &&& (y9 === y10) &&& (y8 === (List.nil ())) &&& (((x9 === (List.nil ())) &&& (y11 === x10)) ||| ((x10 === (x14 % x16)) &&& (x9 === (x14 % x15)) &&& (appendo y11 x15 x16)))) ||| ((x7 === x12) &&& (x5 === x11) &&& (y12 === (x11 % x13)) &&& (y10 === (x5 % x7)) &&& (y8 === (x5 % x6)) &&& (_appendoAppendo x6 y9 x12 y11 x13)))))) 
  in    (_appendoAppendo x0 x1 x2 x3 x4)
