open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec containso y0 = (fresh (q1 q2 q3 q4) ((((y0 === (((Nat.succ Nat.zero)) % ((q1 % q2)))) &&& (newoCono q1 q2)) ||| ((y0 === (q3 % q4)) &&& (_cono q4))))) 
  and newoCono y3 y4 = (fresh (q1 q2) ((((y4 === (q1 % q2)) &&& (y3 === Nat.zero) &&& (_newoCono q1 q2)) ||| (__appendoAppendoAppendoCono y3 y4)))) 
  and _newoCono y7 y8 = (appendoAppendoAppendoCono y7 y8) 
  and appendoAppendoAppendoCono y12 y13 = ((_cono y13) ||| (_appendoAppendoAppendoCono y12 y13)) 
  and _appendoAppendoAppendoCono y17 y18 = (fresh (q1 q2) (((y18 === (q1 % q2)) &&& (newoCono q1 q2) &&& (appendo y17)))) 
  and appendo y22 = (y22 === (Nat.succ Nat.zero)) 
  and __appendoAppendoAppendoCono y26 y27 = (fresh (q1 q2) (((_cono y27) ||| ((y27 === (q1 % q2)) &&& (newoCono q1 q2) &&& (appendo y26))))) 
  and _cono y30 = (fresh (q1 q2 q3 q4) ((((y30 === (((Nat.succ Nat.zero)) % ((q1 % q2)))) &&& (newoCono q1 q2)) ||| ((y30 === (q3 % q4)) &&& (_cono q4))))) 
  in         (containso x0)
