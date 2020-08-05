open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 y4 = 
  let rec p z1 z2 z3 z4 = (fresh (fT fR fZ fY fX__) (((z1 === fX__) &&& ((z2 === fY) &&& ((z3 === fZ) &&& ((z4 === fR) &&& (p_ fX__ fY fT fZ fR))))))) 
  and p_ z1 z2 z3 z4 z5 = ((fresh (fTy_ fZ fTy fY fT_ fH) (((z1 === (fH % fT_)) &&& ((z2 === fY) &&& ((z3 === (fH % fTy)) &&& ((z4 === fZ) &&& ((z5 === (fH % fTy_)) &&& (p_ fT_ fY fTy fZ fTy_)))))))) ||| (fresh (fR fZ fT) (((z1 === (List.nil ())) &&& ((z2 === fT) &&& ((z3 === fT) &&& ((z4 === fZ) &&& ((z5 === fR) &&& (p__ fT fZ fR))))))))) 
  and p__ z1 z2 z3 = ((fresh (fTy fZ fT_ fH) (((z1 === (fH % fT_)) &&& ((z2 === fZ) &&& ((z3 === (fH % fTy)) &&& (p__ fT_ fZ fTy)))))) ||| (fresh (fR) (((z1 === (List.nil ())) &&& ((z2 === fR) &&& (z3 === fR)))))) 
  in    (p y1 y2 y3 y4)
