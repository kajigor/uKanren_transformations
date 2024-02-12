open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec applasto y0 = (fresh (q1 q2) ((((y0 === (((Nat.succ Nat.zero)) % q1)) &&& (_appendo q1 ((List.nil ())))) ||| ((y0 === (q2 % q1)) &&& (appendoLasto q1))))) 
  and appendoLasto y1 = (fresh (q1 q2 q3) (((y1 === (q1 % q2)) &&& (_appendo q2 q3) &&& (lasto ((q1 % q3)))))) 
  and _appendo y3 y4 = (fresh (q1 q2 q3) ((((y4 === (Nat.zero % ((List.nil ())))) &&& (y3 === (List.nil ()))) ||| ((y4 === (q1 % q2)) &&& (y3 === (q1 % q3)) &&& (_appendo q3 q2))))) 
  and lasto y5 = (fresh (q1 q2) (((y5 === (((Nat.succ Nat.zero)) % ((List.nil ())))) ||| ((y5 === (q1 % q2)) &&& (lasto q2))))) 
  in     (applasto x0)
