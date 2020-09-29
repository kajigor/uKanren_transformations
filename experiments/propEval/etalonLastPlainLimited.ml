open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 = 
  let rec evaloT z1 z2 z3 = ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (conj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloT fSt fX fN) &&& (evaloT fSt fY fN))))))) ||| ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloT fSt fX fN) &&& (evaloT fSt fY fN))))))) ||| ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloT fSt fX fN) &&& (evaloF fSt fY fN))))))) ||| ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloF fSt fX fN) &&& (evaloT fSt fY fN))))))) ||| ((fresh (fN fX fSt) (((z1 === fSt) &&& ((z2 === (neg fX)) &&& ((z3 === (Nat.succ fN)) &&& (evaloF fSt fX fN)))))) ||| (fresh (fZ fSt) (((z1 === fSt) &&& ((z2 === (var fZ)) &&& ((z3 === Nat.zero) &&& (elemoT fSt fZ))))))))))) 
  and evaloF z1 z2 z3 = ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (conj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloF fSt fX fN) &&& (evaloF fSt fY fN))))))) ||| ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (conj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloT fSt fX fN) &&& (evaloF fSt fY fN))))))) ||| ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (conj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloF fSt fX fN) &&& (evaloT fSt fY fN))))))) ||| ((fresh (fN fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((z3 === (Nat.succ fN)) &&& ((evaloF fSt fX fN) &&& (evaloF fSt fY fN))))))) ||| ((fresh (fN fX fSt) (((z1 === fSt) &&& ((z2 === (neg fX)) &&& ((z3 === (Nat.succ fN)) &&& (evaloT fSt fX fN)))))) ||| (fresh (fZ fSt) (((z1 === fSt) &&& ((z2 === (var fZ)) &&& ((z3 === Nat.zero) &&& (elemoF fSt fZ))))))))))) 
  and elemoT z1 z2 = ((fresh (fQ1) (((z1 === (!!true % fQ1)) &&& (z2 === Nat.zero)))) ||| (fresh (fQ2 fQ1 fQ3) (((z1 === (fQ3 % fQ1)) &&& ((z2 === (Nat.succ fQ2)) &&& (elemoT fQ1 fQ2)))))) 
  and elemoF z1 z2 = ((fresh (fQ1) (((z1 === (!!false % fQ1)) &&& (z2 === Nat.zero)))) ||| (fresh (fQ2 fQ1 fQ3) (((z1 === (fQ3 % fQ1)) &&& ((z2 === (Nat.succ fQ2)) &&& (elemoF fQ1 fQ2)))))) 
  in     (evaloT y1 y2 y3)
