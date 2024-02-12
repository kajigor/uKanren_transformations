open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec sorto y0 = (fresh (q1 q2 q3 q4) ((((y0 === (q1 % ((q2 % ((q3 % ((q4 % ((List.nil ())))))))))) &&& (minmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4)) ||| ((y0 === (q1 % ((q2 % ((q3 % ((q4 % ((List.nil ())))))))))) &&& (_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4))))) 
  and minmaxoMinmaxoMinmaxoMinmaxoMinmaxo y2 y5 y8 y9 = (fresh (q1) ((((minmaxoMinmaxoMinmaxo q1 y5 y8 y9) &&& (____minmaxo y2 q1)) ||| ((_minmaxoMinmaxoMinmaxo q1 y5 y8 y9) &&& (_______minmaxo y2 q1))))) 
  and minmaxoMinmaxoMinmaxo y11 y12 y15 y16 = ((minmaxoMinmaxo y11 y12 y15 y16) ||| (_minmaxoMinmaxo y11 y12 y15 y16)) 
  and minmaxoMinmaxo y18 y19 y21 y22 = (((y18 === y19) &&& (leo y19) &&& (minmaxo y21 y22)) ||| ((y19 === (succ ((zero ())))) &&& (gtoMinmaxo y18 y21 y22))) 
  and leo y23 = ((y23 === (zero ())) ||| (y23 === (succ ((zero ()))))) 
  and minmaxo y24 y25 = (((y25 === (succ ((succ ((zero ())))))) &&& (y24 === (succ ((zero ())))) &&& (leo ((zero ())))) ||| ((y25 === (succ ((zero ())))) &&& (y24 === (succ ((succ ((zero ())))))))) 
  and gtoMinmaxo y26 y27 y28 = ((y26 === (zero ())) &&& (_minmaxo y27 y28)) 
  and _minmaxo y29 y30 = (((y30 === (succ ((succ ((zero ())))))) &&& (y29 === (zero ()))) ||| ((y30 === (zero ())) &&& (y29 === (succ ((succ ((zero ())))))))) 
  and _minmaxoMinmaxo y31 y32 y34 y35 = (((y31 === y32) &&& (_leo y32) &&& (__minmaxo y34 y35)) ||| ((y32 === (succ ((succ ((zero ())))))) &&& (_gtoMinmaxo y31 y34 y35))) 
  and _leo y36 = (fresh (q1) (((y36 === (zero ())) ||| ((y36 === (succ q1)) &&& (leo q1))))) 
  and __minmaxo y37 y38 = ((y38 === (succ ((zero ())))) &&& (y37 === (succ ((succ ((zero ())))))) &&& (leo ((succ ((succ ((zero ())))))))) 
  and _gtoMinmaxo y39 y40 y41 = (fresh (q1) ((((y39 === (zero ())) &&& (_______minmaxo y40 y41)) ||| ((y39 === (succ q1)) &&& (__gtoMinmaxo y40 y41 q1))))) 
  and __gtoMinmaxo y42 y43 y44 = ((y44 === (zero ())) &&& (___minmaxo y42 y43)) 
  and ___minmaxo y45 y46 = ((y46 === (succ ((zero ())))) &&& (y45 === (succ ((zero ())))) &&& (leo ((succ ((zero ())))))) 
  and ____minmaxo y47 y48 = ((y48 === (zero ())) &&& (y47 === (zero ()))) 
  and _minmaxoMinmaxoMinmaxo y49 y50 y53 y54 = ((__minmaxoMinmaxo y49 y50 y53 y54) ||| (___minmaxoMinmaxo y49 y50 y53 y54)) 
  and __minmaxoMinmaxo y56 y57 y59 y60 = ((y57 === (zero ())) &&& (y56 === y57) &&& (_minmaxo y59 y60)) 
  and ___minmaxoMinmaxo y61 y62 y64 y65 = (((y61 === y62) &&& (_leo y62) &&& (_____minmaxo y64 y65)) ||| ((y62 === (succ ((succ ((zero ())))))) &&& (___gtoMinmaxo y61 y64 y65))) 
  and ___gtoMinmaxo y66 y67 y68 = (fresh (q1) ((((y66 === (zero ())) &&& (____minmaxo y67 y68)) ||| ((y66 === (succ q1)) &&& (____gtoMinmaxo y67 y68 q1))))) 
  and ____gtoMinmaxo y69 y70 y71 = ((y71 === (zero ())) &&& (______minmaxo y69 y70)) 
  and _______minmaxo y72 y73 = (((y73 === (succ ((zero ())))) &&& (y72 === (zero ())) &&& (leo ((zero ())))) ||| ((y73 === (zero ())) &&& (y72 === (succ ((zero ())))))) 
  and _minmaxoMinmaxoMinmaxoMinmaxoMinmaxo y75 y78 y81 y82 = (fresh (q1 q2 q3 q4) ((((__minmaxoMinmaxoMinmaxo q1 y78 q2 q3 y81 y82 q4) &&& (____minmaxo y75 q1)) ||| ((___minmaxoMinmaxoMinmaxo q1 y78 y81 y82) &&& (_minmaxo y75 q1))))) 
  and ___minmaxoMinmaxoMinmaxo y84 y85 y88 y89 = ((____minmaxoMinmaxo y84 y85 y88 y89) ||| (_____minmaxoMinmaxo y84 y85 y88 y89)) 
  and ____minmaxoMinmaxo y91 y92 y94 y95 = ((y92 === (zero ())) &&& (y91 === y92) &&& (_______minmaxo y94 y95)) 
  and _____minmaxoMinmaxo y96 y97 y99 y100 = (((y96 === y97) &&& (leo y97) &&& (______minmaxo y99 y100)) ||| ((y97 === (succ ((zero ())))) &&& (_____gtoMinmaxo y96 y99 y100))) 
  and _____gtoMinmaxo y101 y102 y103 = ((y101 === (zero ())) &&& (____minmaxo y102 y103)) 
  in                           (sorto x0)
