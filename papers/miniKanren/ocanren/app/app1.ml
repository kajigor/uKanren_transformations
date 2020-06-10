open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec appendo y0 y1 y2 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y0 === Std.List.nil () &&& (y1 === y2)
      ||| ( y0 === Std.( % ) q1 q2
          &&& (y2 === Std.( % ) q1 q3)
          &&& (q2 === Std.List.nil () &&& (y1 === q3) ||| (q2 === Std.( % ) q4 q5 &&& (q3 === Std.( % ) q4 q6) &&& _appendo y1 q5 q6)) ) )
  and _appendo y3 y4 y5 =
    fresh (q1 q2 q3) (y4 === Std.List.nil () &&& (y3 === y5) ||| (y4 === Std.( % ) q1 q2 &&& (y5 === Std.( % ) q1 q3) &&& _appendo y3 q2 q3))
  in
  appendo x0 x1 x2
