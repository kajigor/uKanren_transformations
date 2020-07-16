open OCanren
open GT
open Helper

(* Fresh in every disjunct! *)
let topLevel y1 y2 =
  let rec eval z1 z2 z3 = fresh (fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === !!true) &&& eval__1 fA fB)
  and eval__1 z1 z2 =
    fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!true)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& eval__1 fA fB &&& eval__1 fA fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& or_conj__3 fA fB fC)
    ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__4 fA fB)
  and elem__2 z1 z2 z3 z4 =
    fresh (fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fA))
    ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem__2 fA fC fD fE)
  and or_conj__3 z1 z2 z3 =
    fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__1 fA fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__4 fA fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__1 fA fC)
  and eval__4 z1 z2 =
    fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!false)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& and_conj__5 fA fB fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& eval__4 fA fB &&& eval__4 fA fC)
    ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__1 fA fB)
  and and_conj__5 z1 z2 z3 =
    fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__4 fA fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__1 fA fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__4 fA fC)
  in
  eval__1 y1 y2

(* (* Toplevel fresh wherever possible! *)
let topLevel y1 y2 =
  let rec eval z1 z2 z3 = fresh (fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === !!true) &&& eval__1 fA fB)
  and eval__1 z1 z2 =
    fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!true)
    ||| fresh (fC fB fA) ((z1 === fA &&& (z2 === conj fB fC) &&& eval__1 fA fB &&& eval__1 fA fC)
    ||| (z1 === fA &&& (z2 === disj fB fC) &&& or_conj__3 fA fB fC)
    ||| (z1 === fA &&& (z2 === neg fB) &&& eval__4 fA fB))
  and elem__2 z1 z2 z3 z4 =
    fresh (fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fA))
    ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem__2 fA fC fD fE)
  and or_conj__3 z1 z2 z3 =
    fresh (fC fB fA) ((z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__1 fA fC)
    |||  (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__4 fA fC)
    |||  (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__1 fA fC))
  and eval__4 z1 z2 =
    fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!false)
    ||| fresh (fC fB fA) ((z1 === fA &&& (z2 === conj fB fC) &&& and_conj__5 fA fB fC)
    ||| (z1 === fA &&& (z2 === disj fB fC) &&& eval__4 fA fB &&& eval__4 fA fC)
    ||| (z1 === fA &&& (z2 === neg fB) &&& eval__1 fA fB))
  and and_conj__5 z1 z2 z3 =
    fresh (fC fB fA) ((z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__4 fA fC)
    |||  (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__1 fA fC)
    |||  (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__4 fA fC))
  in
  eval__1 y1 y2 *)