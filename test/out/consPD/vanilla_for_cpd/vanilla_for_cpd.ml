open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec solve y0 y1 y2 = (solve_atomSolve y0 y1 y2) 
  and solve_atom y3 y4 y5 = (fresh (q1 q2 q3) (((__solve y3 y4 y5) ||| ((y5 === (q1 % q2)) &&& (y3 === (q1 % q3)) &&& (my_clauseSolve q3 y4 q2)) ||| ((y4 === y5) &&& (y3 === (List.nil ())))))) 
  and _solve y6 y7 = (_solve_atom y6 y7) 
  and __solve y8 y9 y10 = (my_clauseSolve y8 y9 y10) 
  and my_clauseSolve y11 y12 y13 = (fresh (q1 q2 q3) (((__solve y11 y12 y13) ||| ((y13 === (q1 % q2)) &&& (y11 === (q1 % q3)) &&& (my_clauseSolve q3 y12 q2)) ||| ((y12 === y13) &&& (y11 === (List.nil ())))))) 
  and _solve_atom y15 y16 = (fresh (q1) ((((y16 === (List.nil ())) &&& (y15 === (((s ())) % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((List.nil ()))))))))))) ||| ((y16 === (((s ())) % q1)) &&& (__solve_atom q1 y15)) ||| (_solve_atom y15 y16)))) 
  and __solve_atom y17 y18 = (fresh (q1) ((((y18 === (((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))) &&& (y17 === (List.nil ()))) ||| ((y17 === (((Nat.succ Nat.zero)) % q1)) &&& (___solve_atom q1 y18)) ||| (__solve_atom y17 y18)))) 
  and ___solve_atom y19 y20 = (fresh (q1) ((((y20 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))) &&& (y19 === (List.nil ()))) ||| ((y19 === (((Nat.succ ((Nat.succ Nat.zero)))) % q1)) &&& (____solve_atom q1 y20)) ||| (___solve_atom y19 y20)))) 
  and ____solve_atom y21 y22 = (fresh (q1) ((((y22 === (((Nat.succ Nat.zero)) % ((List.nil ())))) &&& (y21 === (List.nil ()))) ||| ((y21 === (((Nat.succ Nat.zero)) % q1)) &&& (_____solve_atom q1 y22)) ||| (____solve_atom y21 y22)))) 
  and _____solve_atom y23 y24 = (((y24 === (List.nil ())) &&& (y23 === (List.nil ()))) ||| (_____solve_atom y23 y24)) 
  and solve_atomSolve y25 y26 y27 = (fresh (q1) ((((solve_atom y25 y26 q1) &&& (_solve y27 q1)) ||| (solve_atomSolve y25 y26 y27)))) 
  in            (solve x0 x1 x2)
