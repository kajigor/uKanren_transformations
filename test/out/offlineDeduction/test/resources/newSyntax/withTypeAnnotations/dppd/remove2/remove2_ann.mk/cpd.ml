open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec rr y0 = (fresh (q1 q2 q3) (((y0 === (q1 % ((q2 % q3)))) &&& (gG q1 q2 q3)))) 
  and gG y1 y5 y6 = (fresh (q1 q2) ((((y6 === (q1 % q2)) &&& (y1 === y5) &&& (hG y5 q1 q2)) ||| (_neqHG y1 y5 y6)))) 
  and hG y9 y10 y11 = (fresh (q1 q2) ((((y11 === (List.nil ())) &&& (_______g y9 y10 y10 ((List.nil ())) ((List.nil ())))) ||| ((y11 === (q1 % q2)) &&& (_gG y9 y10 q1 q2))))) 
  and _gG y14 y15 y16 y17 = (((y15 === y16) &&& (fG y14 y17 y16)) ||| (neqHG y14 y15 y16 y17)) 
  and fG y18 y19 y20 = (fresh (q1 q2) ((((y19 === (List.nil ())) &&& (_______g y18 y20 y20 ((List.nil ())) ((List.nil ())))) ||| ((y19 === (q1 % q2)) &&& (__hG y18 q2 q1 y20 y20))))) 
  and f y38 y39 = (fresh (q1 q2) ((((y39 === (List.nil ())) &&& (y38 === (List.nil ()))) ||| ((y38 === (q1 % q2)) &&& (h q1 q2 y39))))) 
  and _neqG y46 = (fresh (q1) ((_____g ((zero ())) ((((zero ())) % ((((succ ((succ ((zero ())))))) % ((((zero ())) % ((((zero ())) % ((List.nil ())))))))))) q1 y46))) 
  and h y49 y50 y51 = (fresh (q1 q2) ((((y51 === (y49 % ((List.nil ())))) &&& (y50 === (List.nil ()))) ||| ((y50 === (q1 % q2)) &&& (_____g y49 y51 q1 q2))))) 
  and _____g y52 y53 y54 y55 = (fresh (q1) ((((y53 === (y54 % q1)) &&& (y52 === y54) &&& (f y55 q1)) ||| ((y53 === (y52 % q1)) &&& (neq y52 y54) &&& (h y54 y55 q1))))) 
  and neq y56 y57 = (fresh (q1 q2 q3) ((((y57 === (succ q1)) &&& (y56 === (zero ()))) ||| ((y57 === (zero ())) &&& (y56 === (succ q1))) ||| ((y57 === (succ q2)) &&& (y56 === (succ q3)) &&& (neq q3 q2))))) 
  and neqHG y58 y59 y61 y62 = (fresh (q1 q2 q3) ((((y61 === (succ q1)) &&& (y59 === (zero ())) &&& (__hG y58 y62 ((succ q1)) ((zero ())) ((zero ())))) ||| ((y61 === (zero ())) &&& (y59 === (succ q1)) &&& (__hG y58 y62 ((zero ())) ((succ q1)) ((succ q1)))) ||| ((y61 === (succ q2)) &&& (__hG y58 y62 ((succ q2)) ((succ q3)) ((succ q3))) &&& (y59 === (succ q3)) &&& (neq q3 q2))))) 
  and __hG y63 y65 y66 y67 y68 = (fresh (q1 q2) ((((y65 === (List.nil ())) &&& (_______g y63 y67 y68 ((y66 % ((List.nil ())))) ((y66 % ((List.nil ())))))) ||| ((y65 === (q1 % q2)) &&& (___gG y63 y66 y67 y68 q1 q2))))) 
  and ___gG y73 y75 y76 y77 y78 y79 = (fresh (q1) ((((y75 === y78) &&& (_fG y73 y76 y77 y79 y78)) ||| ((neq y75 y78) &&& (h y78 y79 q1) &&& (_______g y73 y76 y77 ((y75 % q1)) ((y75 % q1))))))) 
  and _fG y80 y81 y82 y83 y84 = (fresh (q1 q2 q3) ((((y83 === (List.nil ())) &&& (_______g y80 y81 y82 ((y84 % ((List.nil ())))) ((y84 % ((List.nil ())))))) ||| ((y83 === (q1 % q2)) &&& (h q1 q2 q3) &&& (_______g y80 y81 y82 ((y84 % q3)) ((y84 % q3))))))) 
  and _______g y86 y87 y88 y89 y90 = (fresh (q1 q2) ((((y87 === y86) &&& (y86 === (succ ((zero ())))) &&& (f y90 ((((zero ())) % ((((succ ((succ ((zero ())))))) % ((((zero ())) % ((((zero ())) % ((List.nil ())))))))))))) ||| ((y89 === (q1 % q2)) &&& (neq ((succ ((zero ())))) y87) &&& (y86 === (succ ((zero ())))) &&& (_____g y88 ((((zero ())) % ((((succ ((succ ((zero ())))))) % ((((zero ())) % ((((zero ())) % ((List.nil ())))))))))) q1 q2))))) 
  and _neqHG y93 y94 y95 = (fresh (q1 q2 q3) ((((y94 === (succ q1)) &&& (y93 === (zero ())) &&& (hG ((zero ())) ((succ q1)) y95)) ||| ((y94 === (zero ())) &&& (y93 === (succ q1)) &&& (hG ((succ q1)) ((zero ())) y95)) ||| ((y94 === (succ q2)) &&& (hG ((succ q3)) ((succ q2)) y95) &&& (y93 === (succ q3)) &&& (neq q3 q2))))) 
  in                 (rr x0)
