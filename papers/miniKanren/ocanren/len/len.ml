open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 =
  let rec lengtho x l =
    (x === Std.List.nil () &&& (l === Std.Nat.zero))
    |||
    (fresh (z t h) (x === Std.( % ) h t &&& ((l === Std.Nat.succ z) &&& (lengtho t z))))
    in
  lengtho x0 x1
