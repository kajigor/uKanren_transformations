open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = let rec sorto x y = (((x === (Std.List.nil ())) &&& (y === (Std.List.nil ()))) ||| (fresh (xs' xs s) (((y === (Std.(%) s xs')) &&& ((sorto xs xs') &&& (smallesto x s xs)))))) and smallesto l s l' = (((l === (Std.(%) s (Std.List.nil ()))) &&& (l' === (Std.List.nil ()))) ||| (fresh (max t' s' t h) (((l' === (Std.(%) max t')) &&& ((l === (Std.(%) h t)) &&& ((minmaxo h s' s max) &&& (smallesto t s' t'))))))) and minmaxo a b min max = (((min === a) &&& ((max === b) &&& (leo a b !!true))) ||| ((max === a) &&& ((min === b) &&& (gto a b !!true)))) and leo x y b = (((x === Std.Nat.zero) &&& (b === !!true)) ||| ((fresh (zz) (((x === (Std.Nat.succ (zz))) &&& ((y === Std.Nat.zero) &&& (b === !!false))))) ||| (fresh (y' x') (((x === (Std.Nat.succ (x'))) &&& ((y === (Std.Nat.succ (y'))) &&& (leo x' y' b))))))) and gto x y b = ((fresh (zz) (((x === (Std.Nat.succ (zz))) &&& ((y === Std.Nat.zero) &&& (b === !!true))))) ||| (((x === Std.Nat.zero) &&& (b === !!false)) ||| (fresh (y' x') (((x === (Std.Nat.succ (x'))) &&& ((y === (Std.Nat.succ (y'))) &&& (gto x' y' b))))))) in      sorto x0 x1
