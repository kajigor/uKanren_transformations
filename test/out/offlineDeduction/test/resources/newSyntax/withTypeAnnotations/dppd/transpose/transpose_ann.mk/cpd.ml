open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec transpose y0 = (fresh (q1 q2) (((y0 === (((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))) % ((((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ ((Nat.succ Nat.zero)))) % q1)))))) % ((((((Nat.succ ((Nat.succ Nat.zero)))) % ((Nat.zero % ((Nat.zero % q2)))))) % ((List.nil ())))))))) &&& (nullrows q1 q2)))) 
  and nullrows y1 y2 = ((y2 === (List.nil ())) &&& (y1 === (List.nil ()))) 
  in   (transpose x0)
