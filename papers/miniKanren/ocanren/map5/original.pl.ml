open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 y4 = let rec map5 z1 z2 z3 z4 = ((fresh ((fXs fA fI fF)) ((((((z1 === fF) &&& (z2 === fI)) &&& (z3 === fA)) &&& (z4 === fXs)) &&& (geo fI ((s ((s ((s ((s ((s ((o ())))))))))))) !!true)))) ||| (fresh ((fT fH fA fI fF)) ((((((((z1 === fF) &&& (z2 === fI)) &&& (z3 === fA)) &&& (z4 === (Std.(%) (fH) (fT)))) &&& (lto fI ((s ((s ((s ((s ((s ((o ())))))))))))) !!true)) &&& (apply fF fA fI fH)) &&& (map5 fF ((s (fI))) fA fT))))) in  (map5 y1 y2 y3 y4)
