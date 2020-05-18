open GT
open OCanren
open OCanren.Std
open Helper

let rec eq_nat a b q36 =
  fresh (q37) (q37 === (Pair.pair a b))
    (conde
       [(q37 === (Pair.pair (Nat.zero) (Nat.zero))) &&& (q36 === (!! true));
       fresh (q39) (q37 === (Pair.pair (Nat.succ q39) (Nat.zero))) (q36 === (!! false));
       fresh (q41) (q37 === (Pair.pair (Nat.zero) (Nat.succ q41))) (q36 === (!! false));
       fresh (x y) (q37 === (Pair.pair (Nat.succ x) (Nat.succ y))) (eq_nat x y q36)])

let rec get_term var subst q32 =
  ((subst === (nil ())) &&& (q32 === (none ()))) ||| (fresh (x xs) (subst === ((%) x xs)) (((var === (Nat.zero)) &&& (x === q32)) ||| (fresh (n) (var === (Nat.succ n)) (get_term n xs q32))))

let rec check_uni subst t1 (t2 : (ground_term, logic_term) Logic.injected) q31 =
  let rec forall2 subst l1 l2 q0 =
    fresh (q1) (q1 === (Pair.pair l1 l2))
      (((q1 === (Pair.pair (nil ()) (nil ()))) &&& (q0 === (!! true))) |||
         (fresh (x xs y ys q3 q4) (q1 === (Pair.pair ((%) x xs) ((%) y ys))) (
            check_uni subst x y q3) (forall2 subst xs ys q4) (conde [(q3 === (!! false)) &&& (q0 === (!! false)); (q3 === (!! true)) &&& (q0 === q4)]))) in
  fresh (q11) (q11 === (Pair.pair t1 t2))
    (conde
       [fresh (n1 a1 n2 a2 q12 q13) (q11 === (Pair.pair (constr n1 a1) (constr n2 a2))) (
          eq_nat n1 n2 q12) (forall2 subst a1 a2 q13) (conde [(q12 === (!! false)) &&& (q31 === (!! false)); (q12 === (!! true)) &&& (q31 === q13)]);
       fresh (v n a q19) (q11 === (Pair.pair (var_ v) (constr n a))) (get_term v subst q19)
         (((q19 === (none ())) &&& (q31 === (!! false))) ||| (fresh (t) (q19 === (some t)) (check_uni subst t t2 q31)));
       fresh (n a v q22) (q11 === (Pair.pair (constr n a) (var_ v))) (get_term v subst q22)
         (((q22 === (none ())) &&& (q31 === (!! false))) ||| (fresh (t) (q22 === (some t)) (check_uni subst t1 t q31)));
       fresh (v1 v2 q25) (q11 === (Pair.pair (var_ v1) (var_ v2))) (get_term v1 subst q25)
         ((fresh (t1') (q25 === (some t1')) (check_uni subst t1' t2 q31)) |||
            (fresh (q27) (q25 === (none ())) (get_term v2 subst q27) ((fresh (q28) (q27 === (some q28)) (q31 === (!! false))) ||| ((q27 === (none ())) &&& (eq_nat v1 v2 q31)))))])