open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec fail () = fail 
  and le x y b = (((x === (zero ())) &&& (b === (trueo ()))) ||| (fresh (z) (((x === (succ z)) &&& (y === (zero ())) &&& (b === (falso ()))))) ||| (fresh (x' y') (((x === (succ x')) &&& (y === (succ y')) &&& (le x' y' b))))) 
  and gt x y b = (((x === (zero ())) &&& (b === (falso ()))) ||| (fresh (z) (((x === (succ z)) &&& (y === (zero ())) &&& (b === (trueo ()))))) ||| (fresh (x' y') (((x === (succ x')) &&& (y === (succ y')) &&& (gt x' y' b))))) 
  and maxmin x a i = (((x === (List.nil ())) &&& (a === (zero ())) &&& (i === (zero ()))) ||| (fresh (h t) (((x === (h % t)) &&& (max t h a) &&& (min t h i))))) 
  and max x n m = (((x === (List.nil ())) &&& (m === n)) ||| (fresh (h t) (((x === (h % t)) &&& (((le h n ((trueo ()))) &&& (max t n m)) ||| ((gt h n ((trueo ()))) &&& (max t h m))))))) 
  and min x n m = (((x === (List.nil ())) &&& (m === n)) ||| (fresh (h t) (((x === (h % t)) &&& (((le h n ((trueo ()))) &&& (min t h m)) ||| ((gt h n ((trueo ()))) &&& (min t n m))))))) 
  in       (fresh (a i) ((maxmin ((((succ ((zero ())))) % ((((succ ((succ ((zero ())))))) % ((((succ ((succ ((succ ((zero ())))))))) % ((List.nil ())))))))) a i)))
