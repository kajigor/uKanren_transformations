open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec match y0 = (fresh (q1) (((y0 === (List.nil ())) ||| ((y0 === (((Nat.succ ((Nat.succ Nat.zero)))) % q1)) &&& (match1 q1))))) 
  and match1 y1 = (fresh (q1) (((y1 === (List.nil ())) ||| ((y1 === (((Nat.succ Nat.zero)) % q1)) &&& (_match1 q1))))) 
  and _match1 y2 = (fresh (q1) (((y2 === (List.nil ())) ||| ((y2 === (((Nat.succ Nat.zero)) % q1)) &&& (__match1 q1))))) 
  and __match1 y3 = (fresh (q1) (((y3 === (List.nil ())) ||| ((y3 === (Nat.zero % q1)) &&& (___match1 q1))))) 
  and ___match1 y4 = (fresh (q1) (((y4 === (List.nil ())) ||| ((y4 === (Nat.zero % q1)) &&& (____match1 q1))))) 
  and ____match1 y5 = (fresh (q1) (((y5 === (List.nil ())) ||| ((y5 === (((Nat.succ ((Nat.succ Nat.zero)))) % q1)) &&& (_____match1 q1))))) 
  and _____match1 y6 = (fresh (q1) (((y6 === (List.nil ())) ||| ((y6 === (Nat.zero % q1)) &&& (______match1 q1))))) 
  and ______match1 y7 = ((y7 === (List.nil ())) ||| (y7 === (((Nat.succ Nat.zero)) % ((List.nil ()))))) 
  in         (match x0)
