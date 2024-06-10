open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 x4 = 
  let rec fail () = fail 
  and listo x = ((x === (List.nil ())) ||| (fresh (h t) (((x === (h % t)) &&& (listo t))))) 
  and membero x list = (fresh (h t) (((list === (h % t)) &&& ((x === h) ||| (membero x t))))) 
  and inBotho x ys zs = ((membero x ys) &&& (membero x zs)) 
  and nilo l = (l === (List.nil ())) 
  and singletono l x = (l === (x % ((List.nil ())))) 
  and maxLengtho x m l = ((maxo x m) &&& (lengtho x l)) 
  and maxMino x m l = ((maxo x m) &&& (mino x l)) 
  and copy l c = (((l === (List.nil ())) &&& (c === (List.nil ()))) ||| (fresh (h t t') (((l === (h % t)) &&& (c === (h % t')) &&& (copy t t'))))) 
  and copy2 l c = (((l === (List.nil ())) &&& (c === (List.nil ()))) ||| (fresh (h) (((l === (h % ((List.nil ())))) &&& (c === (h % ((List.nil ()))))))) ||| (fresh (h1 h2 t t') (((l === (h1 % ((h2 % t)))) &&& (c === (h1 % t')) &&& (copy2 t t'))))) 
  and copycopy l l1 l2 = ((copy l l1) &&& (copy2 l l2)) 
  and lengtho x l = (((x === (List.nil ())) &&& (l === (zero ()))) ||| (fresh (h t z) (((x === (h % t)) &&& (l === (succ z)) &&& (lengtho t z))))) 
  and lengtho' x l = (((x === (List.nil ())) &&& (l === (zero ()))) ||| (fresh (h t z) (((x === (h % t)) &&& (lengtho' t z) &&& (l === (succ z)))))) 
  and maxo x m = (maxo1 x ((zero ())) m) 
  and maxo1 x n m = (((x === (List.nil ())) &&& (m === n)) ||| (fresh (h t z) (((x === (h % t)) &&& (leo h n ((trueo ()))) &&& (maxo1 t n m)))) ||| (fresh (h t z) (((x === (h % t)) &&& (gto h n ((trueo ()))) &&& (maxo1 t h m))))) 
  and mino x m = (((x === (List.nil ())) &&& (m === (zero ()))) ||| (fresh (h t) (((x === (h % t)) &&& (mino1 t h m))))) 
  and mino1 x n m = (((x === (List.nil ())) &&& (m === n)) ||| (fresh (h t z) (((x === (h % t)) &&& (leo h n ((trueo ()))) &&& (mino1 t h m)))) ||| (fresh (h t z) (((x === (h % t)) &&& (gto h n ((trueo ()))) &&& (mino1 t n m))))) 
  and appendo x y xy = (((x === (List.nil ())) &&& (y === xy)) ||| (fresh (h t ty) (((x === (h % t)) &&& (xy === (h % ty)) &&& (appendo t y ty))))) 
  and appendo' x y xy = (((x === (List.nil ())) &&& (xy === y)) ||| (fresh (h t ty) (((x === (h % t)) ||| (xy === (h % ty)) ||| (appendo' t y ty))))) 
  and reverso x y = (((x === (List.nil ())) &&& (y === (List.nil ()))) ||| (fresh (h t rt) (((x === (h % t)) &&& (reverso t rt) &&& (appendo rt ((h % ((List.nil ())))) y))))) 
  and doubleReverso xs = (fresh (sx) (((reverso xs sx) &&& (reverso sx xs)))) 
  and revAcco xs acc sx = (((xs === (List.nil ())) &&& (sx === acc)) ||| (fresh (h t) (((xs === (h % t)) &&& (revacco t ((h % acc)) sx))))) 
  and assoco x xs v = (fresh (a b tl) (((xs === (((pair a b)) % tl)) &&& (((a === x) &&& (b === v)) ||| (assoco x tl v))))) 
  and nthOpt xs n r = (fresh (h t x) ((((xs === (List.nil ())) &&& (r === (Option.none ()))) ||| ((xs === (h % t)) &&& (((n === (zero ())) &&& (r === (Option.some h))) ||| ((n === (succ x)) &&& (nthOpt t n r))))))) 
  in                         (fresh (xs ys ts zs rs) (((appendo xs ys ts) &&& (appendo ts zs rs))))
