open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 =
  let rec p z1 z2
    =   ((fresh (fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((p fSt fX) &&& (p___ fSt fY))))))
    ||| ((fresh (fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((p_ fSt fX) &&& (p fSt fY))))))
    ||| ((fresh (fY fX fSt) (((z1 === fSt) &&& ((z2 === (disj fX fY)) &&& ((p fSt fX) &&& (p fSt fY))))))
    ||| (fresh (fY fX fSt) (((z1 === fSt) &&& ((z2 === (conj fX fY)) &&& ((p fSt fX) &&& (p fSt fY)))))
    ||| ((fresh (fZ fSt) (((z1 === fSt) &&& ((z2 === (var fZ)) &&& (p_______ fZ fSt))))) )))
    ||| (fresh (fX fSt) (((z1 === fSt) &&& ((z2 === (neg fX)) &&& (p_____ fSt fX)))))))
  and p_______ z1 z2
    =   ((fresh (fT fH fN1) (((z1 === (Nat.succ fN1)) &&& ((z2 === (fH % fT)) &&& (p_______ fN1 fT)))))
    ||| (fresh (fT) (((z1 === Nat.zero) &&& (z2 === (!!true % fT))))))
  and p_____ z1 z2
    =   ((fresh (fZ fSt) (((z1 === fSt) &&& ((z2 === (var fZ)) &&& (p______ fZ fSt)))))
    ||| ((fresh (fX__ fSt) (((z1 === fSt) &&& ((z2 === (neg fX__)) &&& (p fSt fX__)))))
    ||| ((fresh (fY fX__ fSt) (((z1 === fSt) &&& ((z2 === (disj fX__ fY)) &&& ((p_____ fSt fX__) &&& (p_____ fSt fY))))))
    ||| ((fresh (fY fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY)) &&& ((p_____ fSt fX__) &&& (p_____ fSt fY))))))
    ||| ((fresh (fY fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY)) &&& ((p fSt fX__) &&& (p_____ fSt fY))))))
    ||| (fresh (fY fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY)) &&& ((p_____ fSt fX__) &&& (p fSt fY)))))))))))
  and p______ z1 z2
    =   ((fresh (fT fH fN1) (((z1 === (Nat.succ fN1)) &&& ((z2 === (fH % fT)) &&& (p______ fN1 fT)))))
    ||| (fresh (fT) (((z1 === Nat.zero) &&& (z2 === (!!false % fT))))))
  and p___ z1 z2
    =   ((fresh (fZ fSt) (((z1 === fSt) &&& ((z2 === (var fZ)) &&& (p____ fZ fSt)))))
    ||| ((fresh (fX__ fSt) (((z1 === fSt) &&& ((z2 === (neg fX__)) &&& (p fSt fX__)))))
    ||| ((fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (disj fX__ fY_)) &&& ((p___ fSt fX__) &&& (p___ fSt fY_))))))
    ||| ((fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY_)) &&& ((p___ fSt fX__) &&& (p___ fSt fY_))))))
    ||| ((fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY_)) &&& ((p fSt fX__) &&& (p___ fSt fY_))))))
    ||| (fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY_)) &&& ((p___ fSt fX__) &&& (p fSt fY_)))))))))))
  and p____ z1 z2
    =   ((fresh (fT fH fN1) (((z1 === (Nat.succ fN1)) &&& ((z2 === (fH % fT)) &&& (p____ fN1 fT)))))
    ||| (fresh (fT) (((z1 === Nat.zero) &&& (z2 === (!!false % fT))))))
  and p_ z1 z2
    =   ((fresh (fZ fSt) (((z1 === fSt) &&& ((z2 === (var fZ)) &&& (p__ fZ fSt)))))
    ||| ((fresh (fX__ fSt) (((z1 === fSt) &&& ((z2 === (neg fX__)) &&& (p fSt fX__)))))
    ||| ((fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (disj fX__ fY_)) &&& ((p_ fSt fX__) &&& (p_ fSt fY_))))))
    ||| ((fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY_)) &&& ((p_ fSt fX__) &&& (p_ fSt fY_))))))
    ||| ((fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY_)) &&& ((p fSt fX__) &&& (p_ fSt fY_))))))
    ||| (fresh (fY_ fX__ fSt) (((z1 === fSt) &&& ((z2 === (conj fX__ fY_)) &&& ((p_ fSt fX__) &&& (p fSt fY_)))))))))))
  and p__ z1 z2
    =   ((fresh (fT fH fN1) (((z1 === (Nat.succ fN1)) &&& ((z2 === (fH % fT)) &&& (p__ fN1 fT)))))
    ||| (fresh (fT) (((z1 === Nat.zero) &&& (z2 === (!!false % fT))))))
  in         (p y1 y2)
