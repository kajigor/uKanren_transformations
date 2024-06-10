open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec fail () = fail 
  and leo x y b = (((x === (zero ())) &&& (b === (trueo ()))) ||| (fresh (z) (((x === (succ z)) &&& (y === (zero ())) &&& (b === (falso ()))))) ||| (fresh (x' y') (((x === (succ x')) &&& (y === (succ y')) &&& (leo x' y' b))))) 
  and gto x y b = (((x === (zero ())) &&& (b === (falso ()))) ||| (fresh (z) (((x === (succ z)) &&& (y === (zero ())) &&& (b === (trueo ()))))) ||| (fresh (x' y') (((x === (succ x')) &&& (y === (succ y')) &&& (gto x' y' b))))) 
  and minmaxo a b min max = (((min === a) &&& (b === max) &&& (leo a b ((trueo ())))) ||| ((max === a) &&& (b === min) &&& (gto a b ((trueo ()))))) 
  and smallesto l s l' = (((l === (s % ((List.nil ())))) &&& (l' === (List.nil ()))) ||| (fresh (h t s' t' max) (((l' === (max % t')) &&& (l === (h % t)) &&& (minmaxo h s' s max) &&& (smallesto t s' t'))))) 
  and sorto x y = (((x === (List.nil ())) &&& (y === (List.nil ()))) ||| (fresh (s xs xs') (((y === (s % xs')) &&& (sorto xs xs') &&& (smallesto x s xs))))) 
  in       (fresh (x0) ((sorto x0 ((((zero ())) % ((((succ ((zero ())))) % ((((succ ((succ ((zero ())))))) % ((((succ ((succ ((succ ((zero ())))))))) % ((List.nil ())))))))))))))
