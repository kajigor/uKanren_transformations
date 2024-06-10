open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec help y0 y1 y2 = (fresh (q1 q2 q3) ((((y1 === y2) &&& (y0 === (List.nil ())) &&& (_reverso y2 y2)) ||| ((y2 === (q1 % q2)) &&& (y0 === (q1 % q3)) &&& (_appendoReversoAppendo y1 q3 q2 q1 y1))))) 
  and _reversoAppendoAppendo y12 y14 y15 y17 y18 y19 = (fresh (q1 q2 q3) ((((y14 === y15) &&& (y12 === (List.nil ())) &&& (appendo ((y18 % y19)) y17 ((List.nil ())))) ||| ((y12 === (q1 % q2)) &&& (__reversoAppendo q2 q1 q3) &&& (appendoAppendo q3 y14 y15 y17 y18 y19))))) 
  and _reverso y20 y21 = (fresh (q1 q2) ((((y21 === (List.nil ())) &&& (y20 === (List.nil ()))) ||| ((y20 === (q1 % q2)) &&& (__reversoAppendo q2 q1 y21))))) 
  and appendoAppendo y22 y23 y24 y26 y27 y28 = (fresh (q1) ((((y23 === y24) &&& (y22 === (List.nil ())) &&& (appendo ((y27 % y28)) y26 ((List.nil ())))) ||| ((y22 === (y24 % q1)) &&& (__appendoAppendo y23 y26 q1 ((y27 % y28))))))) 
  and __appendoAppendo y35 y36 y37 y39 = (fresh (q1 q2 q3) ((((y37 === (List.nil ())) &&& (appendo y39 y36 ((y35 % ((List.nil ())))))) ||| ((y39 === (q1 % q2)) &&& (y37 === (q1 % q3)) &&& (__appendoAppendo y35 y36 q3 q2))))) 
  and appendo y40 y41 y42 = (fresh (q1 q2 q3) ((((y42 === (List.nil ())) &&& (y40 === (y41 % ((List.nil ()))))) ||| ((y42 === (q1 % q2)) &&& (y40 === (q1 % q3)) &&& (appendo q3 y41 q2))))) 
  and __reversoAppendo y51 y53 y54 = (fresh (q1 q2 q3) ((((y51 === (List.nil ())) &&& (appendo y54 y53 ((List.nil ())))) ||| ((y51 === (q1 % q2)) &&& (__reversoAppendo q2 q1 q3) &&& (appendo y54 y53 q3))))) 
  and _appendoReversoAppendo y55 y56 y57 y59 y60 = (fresh (q1 q2 q3 q4) ((((y56 === (List.nil ())) &&& (y55 === y57) &&& (__reversoAppendo y57 y59 y60)) ||| ((y57 === (q1 % q2)) &&& (_appendoReversoAppendo y55 q3 q2 q1 q4) &&& (_appendoReversoAppendo y55 q3 q2 q1 q4) &&& (appendo y60 y59 q4))))) 
  in         (help x0 x1 x2)
