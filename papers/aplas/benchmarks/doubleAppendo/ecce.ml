open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 y4 = let rec doubleAppendo z1 z2 z3 z4 = (fresh (fD fC fB fA) (((z1 === fA) &&& ((z2 === fB) &&& ((z3 === fC) &&& ((z4 === fD) &&& (doubleAppendo__1 fA fB fC fD))))))) and doubleAppendo__1 z1 z2 z3 z4 = ((fresh (fC fB fA) (((z1 === (Std.List.nil ())) &&& ((z2 === fA) &&& ((z3 === fB) &&& ((z4 === fC) &&& (appendo__3 fA fB fC))))))) ||| (fresh (fE fD fC fB fA) (((z1 === (Std.(%) (fA) (fB))) &&& ((z2 === fC) &&& ((z3 === fD) &&& ((z4 === (Std.(%) (fA) (fE))) &&& (appendo_conj__2 fB fC fD fE)))))))) and appendo_conj__2 z1 z2 z3 z4 = ((fresh (fC fB fA) (((z1 === (Std.List.nil ())) &&& ((z2 === fA) &&& ((z3 === fB) &&& ((z4 === fC) &&& (appendo__3 fA fB fC))))))) ||| (fresh (fE fD fC fB fA) (((z1 === (Std.(%) (fA) (fB))) &&& ((z2 === fC) &&& ((z3 === fD) &&& ((z4 === (Std.(%) (fA) (fE))) &&& (appendo_conj__2 fB fC fD fE)))))))) and appendo__3 z1 z2 z3 = ((fresh (fA) (((z1 === (Std.List.nil ())) &&& ((z2 === fA) &&& (z3 === fA))))) ||| (fresh (fD fC fB fA) (((z1 === (Std.(%) (fA) (fB))) &&& ((z2 === fC) &&& ((z3 === (Std.(%) (fA) (fD))) &&& (appendo__3 fB fC fD))))))) in     (doubleAppendo y1 y2 y3 y4)
