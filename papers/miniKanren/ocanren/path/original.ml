open OCanren
open OCanren.Std

let rec eqNat a b q23 =
  fresh (q24) (q24 === (pair a b))
    (conde
       [(q24 === (pair (Nat.zero) (Nat.zero))) &&& (q23 === (!! true));
       fresh (q26) (q24 === (pair (Nat.succ q26) (Nat.zero))) (q23 === (!! false));
       fresh (q28) (q24 === (pair (Nat.zero) (Nat.succ q28))) (q23 === (!! false));
       fresh (x y) (q24 === (pair (Nat.succ x) (Nat.succ y))) (eqNat x y q23)])

let eqPair a b q14 =
  fresh (q15 a1 a2 b1 b2 q16 q17) (q15 === (pair a b)) (q15 === (pair (pair a1 a2) (pair b1 b2))) (
    eqNat a1 b1 q16) (eqNat a2 b2 q17) (conde [(q16 === (!! false)) &&& (q14 === (!! false)); (q16 === (!! true)) &&& (q14 === q17)])

let rec elem x g q9 =
  ((g === (nil ())) &&& (q9 === (!! false))) |||
    (fresh (y ys q12) (g === (y % ys)) (eqPair x y q12) (conde [(q12 === (!! true)) &&& (q9 === (!! true)); (q12 === (!! false)) &&& (elem x ys q9)]))

let rec isPath c g q0 =
  (fresh (q1) (c === (q1 % (nil ()))) (q0 === (!! true))) |||
    (fresh (x1 x2 xs q3 q4) (c === (x1 % (x2 % xs))) (elem (pair x1 x2) g q3) (
       isPath (x2 % xs) g q4) (conde [(q3 === (!! false)) &&& (q0 === (!! false)); (q3 === (!! true)) &&& (q0 === q4)]))
