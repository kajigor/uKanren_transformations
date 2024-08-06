open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = 
  let rec doubleAppendo x y z r = (fresh (t) (((appendo x y t) &&& (appendo t z r)))) 
  and appendo x y xy = (((x === (List.nil ())) &&& (xy === y)) ||| (fresh (h t ty) (((x === (h % t)) &&& ((xy === (h % ty)) &&& (appendo t y ty)))))) 
  in   (fresh (x y z r) ((doubleAppendo x y z r)))
