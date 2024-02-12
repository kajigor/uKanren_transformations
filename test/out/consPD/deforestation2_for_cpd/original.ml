open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec fail () = fail 
  and neq x y = ((fresh (t) (((x === Nat.zero) &&& (y === (Nat.succ t))))) ||| (fresh (t) (((x === (Nat.succ t)) &&& (y === Nat.zero)))) ||| (fresh (tx ty) (((x === (Nat.succ tx)) &&& (y === (Nat.succ ty)) &&& (neq tx ty))))) 
  and rr x y = (fresh (t) (((r t y) &&& (r x t)))) 
  and r x y = (((x === (List.nil ())) &&& (y === (List.nil ()))) ||| (fresh (a t t1) (((x === (a % ((a % t)))) &&& (y === (a % t1)) &&& (r t t1)))) ||| (fresh (ax ay t t1) (((x === (ax % ((ay % t)))) &&& (y === (ax % t1)) &&& (neq ax ay) &&& (r ((ay % t)) t1))))) 
  in     (fresh (x) ((rr x ((((Nat.succ Nat.zero)) % ((Nat.zero % ((((s ((s ())) Nat.zero)) % ((Nat.zero % ((Nat.zero % ((List.nil ())))))))))))))))
