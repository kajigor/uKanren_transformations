open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec sorto y0 = (fresh (q1 q2 q3 q4) (((splitoSplitoSortoAppendoAppendo y0 q1 q2) &&& (__sorto q3 q2) &&& (__sorto q4 q1)))) 
  and splitoSplitoSortoAppendoAppendo y1 y4 y9 = (_splitoSplitoSortoAppendoAppendo y1 y4 y9) 
  and _splitoSplitoSortoAppendoAppendo y10 y12 y17 = (__splitoSplitoSortoAppendoAppendo y10 y12 y17) 
  and __splitoSplitoSortoAppendoAppendo y19 y21 y25 = (splitoSortoAppendoAppendo y19 y21 y25) 
  and splitoSortoAppendoAppendo y28 y30 y34 = (_splitoSortoAppendoAppendo y28 y30 y34) 
  and _splitoSortoAppendoAppendo y35 y37 y40 = (fresh (q1) (((y35 === (((zero ())) % q1)) &&& (sortoAppendoAppendo y37 y40 q1)))) 
  and sortoAppendoAppendo y42 y43 y47 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8) ((((y47 === (((zero ())) % q1)) &&& (_appendo q1 ((succ ((zero ())))) y43 y42)) ||| ((splitoAppendo q2 q3 q4 q5 q6 q7) &&& (appendoAppendo y42 y43 q2 y47) &&& (__sorto q5 q6) &&& (__sorto q8 q7))))) 
  and appendoAppendo y51 y52 y53 y55 = (fresh (q1 q2 q3) ((((y53 === (List.nil ())) &&& (_appendo y55 ((succ ((zero ())))) ((((zero ())) % y52)) y51)) ||| ((y55 === (q1 % q2)) &&& (y53 === (q1 % q3)) &&& (appendoAppendo y51 y52 q3 q2))))) 
  and splitoAppendo y56 y57 y58 y59 y61 y62 = (fresh (q1 q2 q3) ((((y59 === (List.nil ())) &&& (y58 === (List.nil ())) &&& (_appendo y56 y57 y61 y62)) ||| ((y59 === (q1 % q2)) &&& (splitoAppendo y56 y57 q3 q2 y61 y62) &&& (y58 === (q1 % q3)) &&& (le y57 q1)) ||| ((y58 === (q1 % q3)) &&& (splitoAppendo y56 y57 q3 y59 y61 y62) &&& (gt y57 q1))))) 
  and _appendo y63 y64 y65 y66 = (fresh (q1 q2 q3) ((((y65 === (List.nil ())) &&& (y63 === (y64 % y66))) ||| ((y65 === (q1 % q2)) &&& (y63 === (q1 % q3)) &&& (_appendo q3 y64 q2 y66))))) 
  and le y67 y68 = (fresh (q1 q2) (((y68 === (zero ())) ||| ((y68 === (succ q1)) &&& (y67 === (succ q2)) &&& (le q2 q1))))) 
  and gt y69 y70 = (fresh (q1 q2 q3) ((((y70 === (succ q1)) &&& (y69 === (zero ()))) ||| ((y70 === (succ q2)) &&& (y69 === (succ q3)) &&& (gt q3 q2))))) 
  and __sorto y73 y74 = (fresh (q1 q2 q3 q4 q5 q6) ((((y74 === (List.nil ())) &&& (y73 === (List.nil ()))) ||| ((y73 === (q1 % q2)) &&& (splitoAppendo y74 q1 q2 q3 q4 q5) &&& (__sorto q3 q4) &&& (__sorto q6 q5))))) 
  in              (sorto x0)
