open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec prune y0 = (fresh (q1 q2 q3) ((((y0 === (tree ((tree ((leaf Nat.zero)) ((Nat.succ Nat.zero)) ((leaf ((Nat.succ Nat.zero)))))) ((Nat.succ Nat.zero)) q1)) &&& (_prune q1)) ||| ((y0 === (tree ((tree ((tree q2 Nat.zero q3)) ((Nat.succ Nat.zero)) ((leaf ((Nat.succ Nat.zero)))))) ((Nat.succ Nat.zero)) q1)) &&& (_prune q1))))) 
  and _prune y1 = (fresh (q1 q2) (((y1 === (leaf Nat.zero)) ||| (y1 === (tree q1 Nat.zero q2))))) 
  in   (prune x0)
