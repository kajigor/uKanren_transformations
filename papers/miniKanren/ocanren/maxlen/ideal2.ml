open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec maxLengtho x m l = ml x Std.Nat.zero m l
  and ml x n m l =
    ((x === Std.List.nil ()) &&& (m === n) &&& (l === Std.Nat.zero))
    ||| (fresh (h t k) ((x === Std.( % ) h t) &&& ((l === Std.Nat.succ k) &&& ((leo h n !!true) &&& (ml t n m k)))))
    ||| (fresh (h t k) ((x === Std.( % ) h t) &&& ((l === Std.Nat.succ k) &&& ((gto h n !!true) &&& (ml t h m k)))))
  and leo x y b =
    x === Std.Nat.zero &&& (b === !!true)
     ||| ( fresh (zz) (x === Std.Nat.succ zz &&& (y === Std.Nat.zero &&& (b === !!false)))
        ||| fresh (y' x') (x === Std.Nat.succ x' &&& (y === Std.Nat.succ y' &&& leo x' y' b)) )
  and gto x y b =
    fresh (zz) (x === Std.Nat.succ zz &&& (y === Std.Nat.zero &&& (b === !!true)))
    ||| ( x === Std.Nat.zero &&& (b === !!false) ||| fresh (y' x') (x === Std.Nat.succ x' &&& (y === Std.Nat.succ y' &&& gto x' y' b)))
  in maxLengtho x0 x1 x2
