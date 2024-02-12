open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec fail () = fail 
  and solve stmts = ((stmts === (List.nil ())) ||| (fresh (a t) (((stmts === (a % t)) &&& (solve_atom a) &&& (solve t))))) 
  and solve_atom a = (fresh (b) (((my_clause a b) &&& (solve b)))) 
  and my_clause clause body = ((fresh (x y z r i) (((clause === (doubleApp x y z r)) &&& (body === (((app x y i)) % ((((app i z r)) % ((List.nil ()))))))))) ||| (fresh (l) (((clause === (app ((List.nil ())) l l)) &&& (body === (List.nil ()))))) ||| (fresh (h x y z) (((clause === (app ((h % x)) y ((h % z)))) &&& (body === (((app x y z)) % ((List.nil ()))))))) ||| (body === (clause % ((List.nil ())))) ||| ((clause === (solve2 ((List.nil ())))) &&& (body === (List.nil ()))) ||| (fresh (a t) (((clause === (solve2 ((a % t)))) &&& (body === (((solve_atom2 a)) % ((((solve2 t)) % ((List.nil ()))))))))) ||| (fresh (a b) (((clause === (solve_atom2 a)) &&& (body === (((my_clause2 a b)) % ((((solve2 b)) % ((List.nil ()))))))))) ||| (fresh (l) (((clause === (my_clause2 ((app ((List.nil ())) l l)) ((List.nil ())))) &&& (body === (List.nil ()))))) ||| (fresh (h x y z) (((clause === (my_clause2 ((app ((h % x)) y ((h % z)))) ((((app x y z)) % ((List.nil ())))))) &&& (body === (List.nil ())))))) 
  and test2 r = (solve_atom ((solve_atom2 ((app ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))))) ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) % ((List.nil ())))))))))) r))))) 
  and test1 r = (solve_atom ((app ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))))) ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) % ((List.nil ())))))))))) r))) 
  in       (fresh (x y z) ((solve ((((doubleApp x y z ((((s ())) % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ Nat.zero)) % ((List.nil ())))))))))))) % ((List.nil ())))))))
