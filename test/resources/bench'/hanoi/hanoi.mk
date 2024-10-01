check state steps q0 = ((
    fresh q1, q2, q7, q9 in
    ((steps == [] &
    get One state q7 &
    isNil q7 q1 &
    get Two state q9 &
    isNil q9 q2 &
    ((q1 == Falso & q0 == Falso) |
      (q1 == Trueo & q0 == q2))))) |
      (fresh x, xs, q11 in
        ((steps == (x :: xs) &
        one_step x state q11 &
        check q11 xs q0))));

one_step step state q13 =
  (fresh fromN, toN, q15, q17, x, xs, q19 in
    ((step == Pair fromN toN &
      q15 == Trueo &
      q17 == (x :: xs) &
      notEqStick fromN toN q15 &
      get fromN state q17 &
      get toN state q19 &
      ((fresh q20 in
        ((q19 == [] &
        set fromN xs state q20 &
        set toN [x] q20 q13))) |
     (fresh y, ys, q23, q24 in
      ((q19 == (y :: ys) &
      q23 == Trueo &
      less x y q23 &
      set fromN xs state q24 &
      set toN (x :: y :: ys) q24 q13)))))));

set name stack state q26 =
 (fresh s1, s2, s3 in
  ((state == Triple s1 s2 s3 &
  ((name == One & q26 == Triple stack s2 s3) |
  (name == Two & q26 == Triple s1 stack s3) |
  (name == Thr & q26 == Triple s1 s2 stack)))));

get name state q31 = (fresh s1, s2, s3 in ((state == Triple s1 s2 s3 & ((name == One & s1 == q31) | (name == Two & s2 == q31) | (name == Thr & s3 == q31)))));
less a b q36 = (fresh b' in (b == S b' & ((a == O & q36 == Trueo) | (fresh a' in (a == S a' & less a' b' q36)))));
isNil l q39 = ((l == [] & q39 == Trueo) | (fresh q41, q42 in ((l == (q41 :: q42) & q39 == Falso))));
notEqStick x y q44 = ((x == One & ((y == One & q44 == Falso) | (y == Two & q44 == Trueo) | (y == Thr & q44 == Trueo))) | (x == Two & ((y == One & q44 == Trueo) | (y == Two & q44 == Falso) | (y == Thr & q44 == Trueo))) | (x == Thr & ((y == One & q44 == Trueo) | (y == Two & q44 == Trueo) | (y == Thr & q44 == Falso))));
fail  = fail [];

? check (Triple (Cons O (Cons (S O) (Cons (S (S O)) (Cons (S (S (S O))) Nil)))) Nil Nil) q Trueo