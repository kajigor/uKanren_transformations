open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = let rec doubleAppendo x y z r = (fresh (t) (((appendo x y t) &&& (appendo t z r)))) and appendo x y xy = (((x === (Std.List.nil ())) &&& (xy === y)) ||| (fresh (ty t h) (((x === (Std.(%) (h) (t))) &&& ((xy === (Std.(%) (h) (ty))) &&& (appendo t y ty)))))) in (doubleAppendo x0 x1 x2 x3)
