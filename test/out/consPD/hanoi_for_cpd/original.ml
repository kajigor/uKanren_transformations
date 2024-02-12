open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec fail () = fail 
  and notEqStick x y q44 = (((x === (one ())) &&& (((y === (one ())) &&& (q44 === (falso ()))) ||| ((y === (two ())) &&& (q44 === (trueo ()))) ||| ((y === (thr ())) &&& (q44 === (trueo ()))))) ||| ((x === (two ())) &&& (((y === (one ())) &&& (q44 === (trueo ()))) ||| ((y === (two ())) &&& (q44 === (falso ()))) ||| ((y === (thr ())) &&& (q44 === (trueo ()))))) ||| ((x === (thr ())) &&& (((y === (one ())) &&& (q44 === (trueo ()))) ||| ((y === (two ())) &&& (q44 === (trueo ()))) ||| ((y === (thr ())) &&& (q44 === (falso ())))))) 
  and isNil l q39 = (((l === (List.nil ())) &&& (q39 === (trueo ()))) ||| (fresh (q41 q42) (((l === (q41 % q42)) &&& (q39 === (falso ())))))) 
  and less a b q36 = (fresh (b') (((b === (Nat.succ b')) &&& (((a === Nat.zero) &&& (q36 === (trueo ()))) ||| (fresh (a') (((a === (Nat.succ a')) &&& (less a' b' q36)))))))) 
  and get name state q31 = (fresh (s1 s2 s3) (((state === (triple s1 s2 s3)) &&& (((name === (one ())) &&& (s1 === q31)) ||| ((name === (two ())) &&& (s2 === q31)) ||| ((name === (thr ())) &&& (s3 === q31)))))) 
  and set name stack state q26 = (fresh (s1 s2 s3) (((state === (triple s1 s2 s3)) &&& (((name === (one ())) &&& (q26 === (triple stack s2 s3))) ||| ((name === (two ())) &&& (q26 === (triple s1 stack s3))) ||| ((name === (thr ())) &&& (q26 === (triple s1 s2 stack))))))) 
  and one_step step state q13 = (fresh (fromN toN q15 q17 x xs q19) (((step === (pair fromN toN)) &&& (q15 === (trueo ())) &&& (q17 === (x % xs)) &&& (notEqStick fromN toN q15) &&& (get fromN state q17) &&& (get toN state q19) &&& ((fresh (q20) (((q19 === (List.nil ())) &&& (set fromN xs state q20) &&& (set toN ((x % ((List.nil ())))) q20 q13)))) ||| (fresh (y ys q23 q24) (((q19 === (y % ys)) &&& (q23 === (trueo ())) &&& (less x y q23) &&& (set fromN xs state q24) &&& (set toN ((x % ((y % ys)))) q24 q13)))))))) 
  and check state steps q0 = ((fresh (q1 q2 q7 q9) (((steps === (List.nil ())) &&& (get ((one ())) state q7) &&& (isNil q7 q1) &&& (get ((two ())) state q9) &&& (isNil q9 q2) &&& (((q1 === (falso ())) &&& (q0 === (falso ()))) ||| ((q1 === (trueo ())) &&& (q0 === q2)))))) ||| (fresh (x xs q11) (((steps === (x % xs)) &&& (one_step x state q11) &&& (check q11 xs q0))))) 
  in         (fresh (q) ((check ((triple ((Nat.zero % ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))))) ((List.nil ())) ((List.nil ())))) q ((trueo ())))))
