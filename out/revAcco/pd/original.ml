open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec revacco xs acc sx = (((xs === (List.nil ())) &&& (sx === acc)) ||| (fresh (h t) (((xs === (h % t)) &&& (revacco t ((h % acc)) sx))))) 
  in  (fresh (x y) ((revacco x ((List.nil ())) y)))
