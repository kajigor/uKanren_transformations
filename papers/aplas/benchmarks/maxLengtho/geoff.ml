open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 = 
  let rec p z1 z2 z3 = (fresh (fL fM fLs) (((z1 === fLs) &&& ((z2 === fM) &&& ((z3 === fL) &&& (p_ fM fLs fL)))))) 
  and p_ z1 z2 z3 = ((fresh (fZ fT fM) (((z1 === fM) &&& ((z2 === (Nat.zero % fT)) &&& ((z3 === (Nat.succ fZ)) &&& (p_ fM fT fZ)))))) ||| ((fresh (fZ fT fZz fM) (((z1 === fM) &&& ((z2 === (((Nat.succ fZz)) % fT)) &&& ((z3 === (Nat.succ fZ)) &&& ((p__ fT fZz fM) &&& (p_____ fT fZ))))))) ||| ((z1 === Nat.zero) &&& ((z2 === (List.nil ())) &&& (z3 === Nat.zero))))) 
  and p__ z1 z2 z3 = ((fresh (fM fZz fT__) (((z1 === (Nat.zero % fT__)) &&& ((z2 === fZz) &&& ((z3 === fM) &&& (p__ fT__ fZz fM)))))) ||| ((fresh (fM fZz fT__ fX1) (((z1 === (((Nat.succ fX1)) % fT__)) &&& ((z2 === fZz) &&& ((z3 === fM) &&& (p___ fX1 fZz fT__ fZz fM)))))) ||| ((fresh (fM fZz fT__ fX1) (((z1 === (((Nat.succ fX1)) % fT__)) &&& ((z2 === fZz) &&& ((z3 === fM) &&& (p____ fX1 fZz fT__ fX1 fM)))))) ||| (fresh (fZz) (((z1 === (List.nil ())) &&& ((z2 === fZz) &&& (z3 === (Nat.succ fZz))))))))) 
  and p___ z1 z2 z3 z4 z5 = ((fresh (fM fX_______ fT__ fX_____) (((z1 === Nat.zero) &&& ((z2 === fX_____) &&& ((z3 === fT__) &&& ((z4 === fX_______) &&& ((z5 === fM) &&& (p__ fT__ fX_______ fM)))))))) ||| (fresh (fM fX_______ fT__ fY1_ fX1_) (((z1 === (Nat.succ fX1_)) &&& ((z2 === (Nat.succ fY1_)) &&& ((z3 === fT__) &&& ((z4 === fX_______) &&& ((z5 === fM) &&& (p___ fX1_ fY1_ fT__ fX_______ fM))))))))) 
  and p____ z1 z2 z3 z4 z5 = ((fresh (fM fX_______ fT__ fY1_ fX1_) (((z1 === (Nat.succ fX1_)) &&& ((z2 === (Nat.succ fY1_)) &&& ((z3 === fT__) &&& ((z4 === fX_______) &&& ((z5 === fM) &&& (p____ fX1_ fY1_ fT__ fX_______ fM)))))))) ||| (fresh (fM fX_______ fT__ fZz_) (((z1 === (Nat.succ fZz_)) &&& ((z2 === Nat.zero) &&& ((z3 === fT__) &&& ((z4 === fX_______) &&& ((z5 === fM) &&& (p__ fT__ fX_______ fM))))))))) 
  and p_____ z1 z2 = ((fresh (fZ_ fT__ fH__) (((z1 === (fH__ % fT__)) &&& ((z2 === (Nat.succ fZ_)) &&& (p_____ fT__ fZ_))))) ||| ((z1 === (List.nil ())) &&& (z2 === Nat.zero))) 
  in       (p y1 y2 y3)
