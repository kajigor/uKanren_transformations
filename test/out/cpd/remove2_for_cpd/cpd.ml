open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec rr y0 = (fresh (q1 q2 q3) (((y0 === (q1 % ((q2 % q3)))) &&& (gG q1 q2 q3)))) 
  and gG y1 y5 y6 = (fresh (q1 q2) ((((y6 === (q1 % q2)) &&& (y1 === y5) &&& (hG y5 q1 q2)) ||| (_neqHG y1 y5 y6)))) 
  and hG y9 y10 y11 = (fresh (q1 q2) ((((y11 === (List.nil ())) &&& (______g y9 y10 y10 ((List.nil ())) ((List.nil ())))) ||| ((y11 === (q1 % q2)) &&& (_gG y9 y10 q1 q2))))) 
  and _gG y14 y15 y16 y17 = (((y15 === y16) &&& (fG y14 y17 y16)) ||| (neqHG y14 y15 y16 y17)) 
  and fG y18 y19 y20 = (fresh (q1 q2) ((((y19 === (List.nil ())) &&& (______g y18 y20 y20 ((List.nil ())) ((List.nil ())))) ||| ((y19 === (q1 % q2)) &&& (__hG y18 q2 q1 y20 y20))))) 
  and f y36 y37 = (fresh (q1 q2) ((((y37 === (List.nil ())) &&& (y36 === (List.nil ()))) ||| ((y36 === (q1 % q2)) &&& (h q1 q2 y37))))) 
  and h y42 y43 y44 = (fresh (q1 q2) ((((y44 === (y42 % ((List.nil ())))) &&& (y43 === (List.nil ()))) ||| ((y43 === (q1 % q2)) &&& (____g y42 y44 q1 q2))))) 
  and ____g y45 y46 y47 y48 = (fresh (q1) ((((y46 === (y47 % q1)) &&& (y45 === y47) &&& (f y48 q1)) ||| ((y46 === (y45 % q1)) &&& (neq y45 y47) &&& (h y47 y48 q1))))) 
  and neq y49 y50 = (fresh (q1 q2 q3) ((((y50 === (succ q1)) &&& (y49 === (zero ()))) ||| ((y50 === (zero ())) &&& (y49 === (succ q1))) ||| ((y50 === (succ q2)) &&& (y49 === (succ q3)) &&& (neq q3 q2))))) 
  and neqHG y51 y52 y54 y55 = (fresh (q1 q2 q3) ((((y54 === (succ q1)) &&& (y52 === (zero ())) &&& (__hG y51 y55 ((succ q1)) ((zero ())) ((zero ())))) ||| ((y54 === (zero ())) &&& (y52 === (succ q1)) &&& (__hG y51 y55 ((zero ())) ((succ q1)) ((succ q1)))) ||| ((y54 === (succ q2)) &&& (__hG y51 y55 ((succ q2)) ((succ q3)) ((succ q3))) &&& (y52 === (succ q3)) &&& (neq q3 q2))))) 
  and __hG y56 y58 y59 y60 y61 = (fresh (q1 q2) ((((y58 === (List.nil ())) &&& (______g y56 y60 y61 ((y59 % ((List.nil ())))) ((y59 % ((List.nil ())))))) ||| ((y58 === (q1 % q2)) &&& (___gG y56 y59 y60 y61 q1 q2))))) 
  and ___gG y66 y68 y69 y70 y71 y72 = (fresh (q1) ((((y68 === y71) &&& (_fG y66 y69 y70 y72 y71)) ||| ((neq y68 y71) &&& (h y71 y72 q1) &&& (______g y66 y69 y70 ((y68 % q1)) ((y68 % q1))))))) 
  and _fG y73 y74 y75 y76 y77 = (fresh (q1 q2 q3) ((((y76 === (List.nil ())) &&& (______g y73 y74 y75 ((y77 % ((List.nil ())))) ((y77 % ((List.nil ())))))) ||| ((y76 === (q1 % q2)) &&& (h q1 q2 q3) &&& (______g y73 y74 y75 ((y77 % q3)) ((y77 % q3))))))) 
  and ______g y79 y80 y81 y82 y83 = (fresh (q1) ((((y80 === y79) &&& (y79 === (succ ((zero ())))) &&& (f y83 ((((zero ())) % ((((succ ((succ ((zero ())))))) % ((((zero ())) % ((((zero ())) % ((List.nil ())))))))))))) ||| ((y80 === (zero ())) &&& (y79 === (succ ((zero ())))) &&& (f ((y81 % y82)) ((((zero ())) % ((((succ ((succ ((zero ())))))) % ((((zero ())) % ((((zero ())) % ((List.nil ())))))))))))) ||| ((y80 === (succ ((succ q1)))) &&& (y79 === (succ ((zero ())))) &&& (f ((y81 % y82)) ((((zero ())) % ((((succ ((succ ((zero ())))))) % ((((zero ())) % ((((zero ())) % ((List.nil ()))))))))))))))) 
  and _neqHG y86 y87 y88 = (fresh (q1 q2 q3) ((((y87 === (succ q1)) &&& (y86 === (zero ())) &&& (hG ((zero ())) ((succ q1)) y88)) ||| ((y87 === (zero ())) &&& (y86 === (succ q1)) &&& (hG ((succ q1)) ((zero ())) y88)) ||| ((y87 === (succ q2)) &&& (hG ((succ q3)) ((succ q2)) y88) &&& (y86 === (succ q3)) &&& (neq q3 q2))))) 
  in                (rr x0)
