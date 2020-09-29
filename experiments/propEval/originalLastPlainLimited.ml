open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y4 =
  let rec evalo z1 z2 z3 z4 = ((fresh (fW fV fN fU fY fX fSt) (((z1 === fSt) &&& ((z2 === (conj fX fY)) &&& ((z3 === fU) &&& ((z4 === (Nat.succ fN)) &&& ((evalo fSt fX fV fN) &&& ((evalo fSt fY fW fN) &&& (ando fV fW fU))))))))) ||| ((fresh (fW fV fN fU fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((z3 === fU) &&& ((z4 === (Nat.succ fN)) &&& ((evalo fSt fX fV fN) &&& ((evalo fSt fY fW fN) &&& (oro fV fW fU))))))))) ||| ((fresh (fV fN fU fX fSt) (((z1 === fSt) &&& ((z2 === (neg fX)) &&& ((z3 === fU) &&& ((z4 === (Nat.succ fN)) &&& ((evalo fSt fX fV fN) &&& (noto fV fU)))))))) ||| (fresh (fU fZ fSt) (((z1 === fSt) &&& ((z2 === (var fZ)) &&& ((z3 === fU) &&& ((z4 === Nat.zero) &&& (elemo fZ fSt fU))))))))))
  and ando z1 z2 z3 = (((z1 === !!true) &&& ((z2 === !!true) &&& (z3 === !!true))) ||| (((z1 === !!false) &&& ((z2 === !!true) &&& (z3 === !!false))) ||| (((z1 === !!true) &&& ((z2 === !!false) &&& (z3 === !!false))) ||| ((z1 === !!false) &&& ((z2 === !!false) &&& (z3 === !!false))))))
  and oro z1 z2 z3 = (((z1 === !!true) &&& ((z2 === !!true) &&& (z3 === !!true))) ||| (((z1 === !!false) &&& ((z2 === !!true) &&& (z3 === !!true))) ||| (((z1 === !!true) &&& ((z2 === !!false) &&& (z3 === !!true))) ||| ((z1 === !!false) &&& ((z2 === !!false) &&& (z3 === !!false))))))
  and noto z1 z2 = (((z1 === !!true) &&& (z2 === !!false)) ||| ((z1 === !!false) &&& (z2 === !!true)))
  and elemo z1 z2 z3 = ((fresh (fT fH) (((z1 === Nat.zero) &&& ((z2 === (fH % fT)) &&& (z3 === fH))))) ||| (fresh (fV fT fH fN1) (((z1 === (Nat.succ fN1)) &&& ((z2 === (fH % fT)) &&& ((z3 === fV) &&& (elemo fN1 fT fV)))))))
  in      (evalo y1 y2 (!!true) y4)
