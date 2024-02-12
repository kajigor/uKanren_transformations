open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec double_appendo y0 y1 y2 = (fresh (q1) ((((y0 === (List.nil ())) &&& (appendo y2 y1)) ||| ((y0 === (((Nat.succ Nat.zero)) % q1)) &&& (appendoAppendo y1 y2 q1))))) 
  and appendo y3 y4 = (fresh (q1) ((((y4 === (List.nil ())) &&& (y3 === (((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) % ((Nat.zero % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ()))))))))))))))))) ||| ((y4 === (((Nat.succ Nat.zero)) % q1)) &&& (_appendo y3 q1))))) 
  and _appendo y5 y6 = (fresh (q1) ((((y6 === (List.nil ())) &&& (y5 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) % ((Nat.zero % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ()))))))))))))))) ||| ((y6 === (((Nat.succ ((Nat.succ Nat.zero)))) % q1)) &&& (__appendo y5 q1))))) 
  and __appendo y7 y8 = (fresh (q1) ((((y8 === (List.nil ())) &&& (y7 === (((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) % ((Nat.zero % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ()))))))))))))) ||| ((y8 === (((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) % q1)) &&& (___appendo y7 q1))))) 
  and ___appendo y9 y10 = (fresh (q1) ((((y10 === (List.nil ())) &&& (y9 === (Nat.zero % ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ()))))))))))) ||| ((y10 === (Nat.zero % q1)) &&& (____appendo y9 q1))))) 
  and ____appendo y11 y12 = (fresh (q1) ((((y12 === (List.nil ())) &&& (y11 === (Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ()))))))))) ||| ((y12 === (Nat.zero % q1)) &&& (_____appendo y11 q1))))) 
  and _____appendo y13 y14 = (fresh (q1) ((((y14 === (List.nil ())) &&& (y13 === (((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ()))))))) ||| ((y14 === (((Nat.succ Nat.zero)) % q1)) &&& (______appendo y13 q1))))) 
  and ______appendo y15 y16 = (((y16 === (List.nil ())) &&& (y15 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ()))))) ||| ((y16 === (((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))) &&& (y15 === (List.nil ())))) 
  and appendoAppendo y17 y18 y19 = (fresh (q1) ((((y19 === (List.nil ())) &&& (_appendo y18 y17)) ||| ((y19 === (((Nat.succ ((Nat.succ Nat.zero)))) % q1)) &&& (_appendoAppendo y17 y18 q1))))) 
  and _appendoAppendo y21 y22 y23 = (fresh (q1) ((((y23 === (List.nil ())) &&& (__appendo y22 y21)) ||| ((y23 === (((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) % q1)) &&& (__appendoAppendo y21 y22 q1))))) 
  and __appendoAppendo y25 y26 y27 = (fresh (q1) ((((y27 === (List.nil ())) &&& (___appendo y26 y25)) ||| ((y27 === (Nat.zero % q1)) &&& (___appendoAppendo y25 y26 q1))))) 
  and ___appendoAppendo y29 y30 y31 = (fresh (q1) ((((y31 === (List.nil ())) &&& (____appendo y30 y29)) ||| ((y31 === (Nat.zero % q1)) &&& (____appendoAppendo y29 y30 q1))))) 
  and ____appendoAppendo y33 y34 y35 = (fresh (q1) ((((y35 === (List.nil ())) &&& (_____appendo y34 y33)) ||| ((y35 === (((Nat.succ Nat.zero)) % q1)) &&& (_____appendoAppendo y33 y34 q1))))) 
  and _____appendoAppendo y37 y38 y39 = (fresh (q1) ((((y39 === (List.nil ())) &&& (______appendo y38 y37)) ||| ((y39 === (((Nat.succ ((Nat.succ Nat.zero)))) % q1)) &&& (y38 === (List.nil ())) &&& (_______appendo y37 q1))))) 
  and _______appendo y41 y42 = ((y42 === (List.nil ())) &&& (y41 === (List.nil ()))) 
  in                (double_appendo x0 x1 x2)
