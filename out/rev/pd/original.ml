open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec reverso x y = (((x === (List.nil ())) &&& (y === (List.nil ()))) ||| (fresh (h t rt) (((x === (h % t)) &&& ((reverso t rt) &&& (appendo rt ((h % ((List.nil ())))) y)))))) 
  and appendo x y xy = (((x === (List.nil ())) &&& (xy === y)) ||| (fresh (h t ty) (((x === (h % t)) &&& ((xy === (h % ty)) &&& (appendo t y ty)))))) 
  in   (fresh (x y) ((reverso x y)))
