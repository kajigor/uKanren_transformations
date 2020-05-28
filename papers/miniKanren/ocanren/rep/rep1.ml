open GT
open OCanren
open OCanren.Std

let topLevel x0 x1 =
  let rec _rep y2 y3 =
    fresh (q1 q2) (y2 === Std.Nat.zero &&& (y3 === Std.List.nil ()) ||| (y2 === Std.Nat.succ q1 &&& (y3 === Std.( % ) Std.Nat.zero q2) &&& _rep q1 q2))
  in
  _rep x0 x1
