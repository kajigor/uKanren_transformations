open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 y4 = let rec doubleAppendo z1 z2 z3 z4 = ((fresh (fR fZ fY fT fH) ((((((z1 === (Std.(%) (fH) (fT))) &&& (z2 === fY)) &&& (z3 === fZ)) &&& (z4 === (Std.(%) (fH) (fR)))) &&& (doubleAppendo fT fY fZ fR)))) ||| (fresh (fR fZ fY) ((((((z1 === (Std.List.nil ())) &&& (z2 === fY)) &&& (z3 === fZ)) &&& (z4 === fR)) &&& (appendo fY fZ fR))))) and appendo z1 z2 z3 = ((fresh (fR fY fT fH) (((((z1 === (Std.(%) (fH) (fT))) &&& (z2 === fY)) &&& (z3 === (Std.(%) (fH) (fR)))) &&& (appendo fT fY fR)))) ||| (fresh (fY) ((((z1 === (Std.List.nil ())) &&& (z2 === fY)) &&& (z3 === fY))))) in   (doubleAppendo y1 y2 y3 y4)
