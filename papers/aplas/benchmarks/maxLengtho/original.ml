open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = let rec maxLengtho x m l = ((maxo x m) &&& (lengtho x l)) and maxo x m = (maxo1 x Std.Nat.zero m) and maxo1 x n m = (((x === (Std.List.nil ())) &&& (m === n)) ||| ((fresh (z t h) (((x === (Std.(%) (h) (t))) &&& ((leo h n !!true) &&& (maxo1 t n m))))) ||| (fresh (z t h) (((x === (Std.(%) (h) (t))) &&& ((gto h n !!true) &&& (maxo1 t h m))))))) and leo x y b = (((x === Std.Nat.zero) &&& (b === !!true)) ||| ((fresh (zz) (((x === (Std.Nat.succ (zz))) &&& ((y === Std.Nat.zero) &&& (b === !!false))))) ||| (fresh (y' x') (((x === (Std.Nat.succ (x'))) &&& ((y === (Std.Nat.succ (y'))) &&& (leo x' y' b))))))) and gto x y b = ((fresh (zz) (((x === (Std.Nat.succ (zz))) &&& ((y === Std.Nat.zero) &&& (b === !!true))))) ||| (((x === Std.Nat.zero) &&& (b === !!false)) ||| (fresh (y' x') (((x === (Std.Nat.succ (x'))) &&& ((y === (Std.Nat.succ (y'))) &&& (gto x' y' b))))))) and lengtho x l = (((x === (Std.List.nil ())) &&& (l === Std.Nat.zero)) ||| (fresh (z t h) (((x === (Std.(%) (h) (t))) &&& ((l === (Std.Nat.succ (z))) &&& (lengtho t z)))))) in       ( ((maxLengtho x0 x1 x2 )))
