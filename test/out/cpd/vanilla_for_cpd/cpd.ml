open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec solve y0 y1 y2 = ((solve_atomSolve_atom y0 y1 y2) ||| (solve y0 y1 y2)) 
  and solve_atomSolve_atom y3 y4 y5 = (fresh (q1 q2 q3) ((((y3 === (List.nil ())) &&& (solve_atom y4 y5 ((((s ())) % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))))) ||| ((y3 === (q1 % q2)) &&& (solve_atom q2 y4 q3) &&& (solve_atom ((q1 % q3)) y5 ((((s ())) % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))))) ||| (solve_atomSolve_atom y3 y4 y5)))) 
  and solve_atom y7 y8 y9 = (fresh (q1 q2 q3) ((((y8 === y9) &&& (y7 === (List.nil ()))) ||| ((y9 === (q1 % q2)) &&& (y7 === (q1 % q3)) &&& (solve_atom q3 y8 q2)) ||| (solve_atom y7 y8 y9)))) 
  in    (solve x0 x1 x2)
