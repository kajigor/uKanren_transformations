open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec prune y0 = (fresh (q1 q2 q3 q4) (((y0 === (tree ((tree ((leaf Nat.zero)) ((Nat.succ Nat.zero)) ((leaf ((Nat.succ Nat.zero)))))) ((Nat.succ Nat.zero)) ((leaf Nat.zero)))) ||| (y0 === (tree ((tree ((leaf Nat.zero)) ((Nat.succ Nat.zero)) ((leaf ((Nat.succ Nat.zero)))))) ((Nat.succ Nat.zero)) ((tree q1 Nat.zero q2)))) ||| (y0 === (tree ((tree ((tree q3 Nat.zero q4)) ((Nat.succ Nat.zero)) ((leaf ((Nat.succ Nat.zero)))))) ((Nat.succ Nat.zero)) ((leaf Nat.zero)))) ||| (y0 === (tree ((tree ((tree q3 Nat.zero q4)) ((Nat.succ Nat.zero)) ((leaf ((Nat.succ Nat.zero)))))) ((Nat.succ Nat.zero)) ((tree q1 Nat.zero q2))))))) 
  in  (prune x0)
