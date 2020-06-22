open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 = let rec type_ z1 z2 z3 = (fresh (fB fA) (((((z1 === fA) &&& (z2 === fB)) &&& (z3 === (Option.some (integer ())))) &&& (type___1 fA fB)))) and type___1 z1 z2 = ((((((fresh (fB fA) (((z1 === iConst_ fA) &&& (z2 === fB)))) ||| (fresh (fC fB fA) ((((z1 === var_ fA) &&& (z2 === (Std.(%) (fB) (fC)))) &&& (idx__4 fA fB fC (integer ())))))) ||| (fresh (fC fB fA) (((((z1 === plus_ fA fB) &&& (z2 === fC)) &&& (type___1 fA fC)) &&& (type___1 fB fC))))) ||| (fresh (fC fB fA) (((((z1 === mult_ fA fB) &&& (z2 === fC)) &&& (type___1 fA fC)) &&& (type___1 fB fC))))) ||| (fresh (fD fC fB fA) ((((z1 === let_ fA fB) &&& (z2 === fC)) &&& (type__conj__5 fA fC fD fB fD))))) ||| (fresh (fD fC fB fA) ((((z1 === if_ fA fB fC) &&& (z2 === fD)) &&& (type__conj__2 fA fD fB (integer ()) fC))))) and type__conj__2 z1 z2 z3 z4 z5 = ((((((fresh (fE fD fC fB fA) ((((((((z1 === bConst_ fA) &&& (z2 === fB)) &&& (z3 === fC)) &&& (z4 === fD)) &&& (z5 === fE)) &&& (type___3 fC fB fD)) &&& (type___3 fE fB fD)))) ||| (fresh (fF fE fD fC fB fA) (((((((((z1 === var_ fA) &&& (z2 === (Std.(%) (fB) (fC)))) &&& (z3 === fD)) &&& (z4 === fE)) &&& (z5 === fF)) &&& (idx__4 fA fB fC (boolean ()))) &&& (type___3 fD ((Std.(%) (fB) (fC))) fE)) &&& (type___3 fF ((Std.(%) (fB) (fC))) fE))))) ||| (fresh (fG fF fE fD fC fB fA) ((((((((((z1 === equal_ fA fB) &&& (z2 === fC)) &&& (z3 === fD)) &&& (z4 === fE)) &&& (z5 === fF)) &&& (type___3 fA fC fG)) &&& (type___3 fB fC fG)) &&& (type___3 fD fC fE)) &&& (type___3 fF fC fE))))) ||| (fresh (fF fE fD fC fB fA) ((((((((((z1 === less_ fA fB) &&& (z2 === fC)) &&& (z3 === fD)) &&& (z4 === fE)) &&& (z5 === fF)) &&& (type___1 fA fC)) &&& (type___1 fB fC)) &&& (type___3 fD fC fE)) &&& (type___3 fF fC fE))))) ||| (fresh (fG fF fE fD fC fB fA) ((((((((((z1 === let_ fA fB) &&& (z2 === fC)) &&& (z3 === fD)) &&& (z4 === fE)) &&& (z5 === fF)) &&& (type___3 fA fC fG)) &&& (type___3 fB ((Std.(%) (fG) (fC))) (boolean ()))) &&& (type___3 fD fC fE)) &&& (type___3 fF fC fE))))) ||| (fresh (fG fF fE fD fC fB fA) (((((((((z1 === if_ fA fB fC) &&& (z2 === fD)) &&& (z3 === fE)) &&& (z4 === fF)) &&& (z5 === fG)) &&& (type__conj__2 fA fD fB (boolean ()) fC)) &&& (type___3 fE fD fF)) &&& (type___3 fG fD fF))))) and type___3 z1 z2 z3 = (((((((((fresh (fB fA) ((((z1 === bConst_ fA) &&& (z2 === fB)) &&& (z3 === boolean ())))) ||| (fresh (fB fA) ((((z1 === iConst_ fA) &&& (z2 === fB)) &&& (z3 === integer ()))))) ||| (fresh (fD fC fB fA) (((((z1 === var_ fA) &&& (z2 === (Std.(%) (fB) (fC)))) &&& (z3 === fD)) &&& (idx__4 fA fB fC fD))))) ||| (fresh (fC fB fA) ((((((z1 === plus_ fA fB) &&& (z2 === fC)) &&& (z3 === integer ())) &&& (type___1 fA fC)) &&& (type___1 fB fC))))) ||| (fresh (fC fB fA) ((((((z1 === mult_ fA fB) &&& (z2 === fC)) &&& (z3 === integer ())) &&& (type___1 fA fC)) &&& (type___1 fB fC))))) ||| (fresh (fD fC fB fA) ((((((z1 === equal_ fA fB) &&& (z2 === fC)) &&& (z3 === boolean ())) &&& (type___3 fA fC fD)) &&& (type___3 fB fC fD))))) ||| (fresh (fC fB fA) ((((((z1 === less_ fA fB) &&& (z2 === fC)) &&& (z3 === boolean ())) &&& (type___1 fA fC)) &&& (type___1 fB fC))))) ||| (fresh (fE fD fC fB fA) ((((((z1 === let_ fA fB) &&& (z2 === fC)) &&& (z3 === fD)) &&& (type___3 fA fC fE)) &&& (type___3 fB ((Std.(%) (fE) (fC))) fD))))) ||| (fresh (fE fD fC fB fA) (((((z1 === if_ fA fB fC) &&& (z2 === fD)) &&& (z3 === fE)) &&& (type__conj__2 fA fD fB fE fC))))) and idx__4 z1 z2 z3 z4 = ((fresh (fB fA) (((((z1 === (Std.Nat.zero)) &&& (z2 === fA)) &&& (z3 === fB)) &&& (z4 === fA)))) ||| (fresh (fE fD fC fB fA) ((((((z1 === (Std.Nat.succ (fA))) &&& (z2 === fB)) &&& (z3 === (Std.(%) (fC) (fD)))) &&& (z4 === fE)) &&& (idx__4 fA fC fD fE))))) and type__conj__5 z1 z2 z3 z4 z5 = (((((((((fresh (fD fC fB fA) (((((((z1 === bConst_ fA) &&& (z2 === fB)) &&& (z3 === boolean ())) &&& (z4 === fC)) &&& (z5 === fD)) &&& (type___1 fC ((Std.(%) (fD) (fB))))))) ||| (fresh (fD fC fB fA) (((((((z1 === iConst_ fA) &&& (z2 === fB)) &&& (z3 === integer ())) &&& (z4 === fC)) &&& (z5 === fD)) &&& (type___1 fC ((Std.(%) (fD) (fB)))))))) ||| (fresh (fF fE fD fC fB fA) ((((((((z1 === var_ fA) &&& (z2 === (Std.(%) (fB) (fC)))) &&& (z3 === fD)) &&& (z4 === fE)) &&& (z5 === fF)) &&& (idx__4 fA fB fC fD)) &&& (type___1 fE ((Std.(%) (fF) ((Std.(%) (fB) (fC)))))))))) ||| (fresh (fE fD fC fB fA) (((((((((z1 === plus_ fA fB) &&& (z2 === fC)) &&& (z3 === integer ())) &&& (z4 === fD)) &&& (z5 === fE)) &&& (type___1 fA fC)) &&& (type___1 fB fC)) &&& (type___1 fD ((Std.(%) (fE) (fC)))))))) ||| (fresh (fE fD fC fB fA) (((((((((z1 === mult_ fA fB) &&& (z2 === fC)) &&& (z3 === integer ())) &&& (z4 === fD)) &&& (z5 === fE)) &&& (type___1 fA fC)) &&& (type___1 fB fC)) &&& (type___1 fD ((Std.(%) (fE) (fC)))))))) ||| (fresh (fF fE fD fC fB fA) ((((((((z1 === equal_ fA fB) &&& (z2 === fC)) &&& (z3 === boolean ())) &&& (z4 === fD)) &&& (z5 === fE)) &&& (type___3 fA fC fF)) &&& (type__conj__5 fB fC fF fD fE))))) ||| (fresh (fE fD fC fB fA) (((((((((z1 === less_ fA fB) &&& (z2 === fC)) &&& (z3 === boolean ())) &&& (z4 === fD)) &&& (z5 === fE)) &&& (type___1 fA fC)) &&& (type___1 fB fC)) &&& (type___1 fD ((Std.(%) (fE) (fC)))))))) ||| (fresh (fG fF fE fD fC fB fA) (((((((((z1 === let_ fA fB) &&& (z2 === fC)) &&& (z3 === fD)) &&& (z4 === fE)) &&& (z5 === fF)) &&& (type___3 fA fC fG)) &&& (type___3 fB ((Std.(%) (fG) (fC))) fD)) &&& (type___1 fE ((Std.(%) (fF) (fC)))))))) ||| (fresh (fG fF fE fD fC fB fA) ((((((((z1 === if_ fA fB fC) &&& (z2 === fD)) &&& (z3 === fE)) &&& (z4 === fF)) &&& (z5 === fG)) &&& (type__conj__2 fA fD fB fE fC)) &&& (type___1 fF ((Std.(%) (fG) (fD)))))))) in       (type___1 y1 y2)
