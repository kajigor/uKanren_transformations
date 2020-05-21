open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec check_uni y0 y1 y2 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15)
      ( y1 === constr Nat.zero q1 &&& (y2 === constr Nat.zero q2) &&& forall2 y0 q1 q2
      ||| (y1 === constr (Nat.succ q3) q1 &&& (y2 === constr (Nat.succ q4) q2) &&& (eq_nat q3 q4 &&& forall2 y0 q1 q2))
      ||| (y1 === var_ Nat.zero &&& (y2 === constr q5 q6) &&& (y0 === Option.some q7 % q8) &&& check_uni (Option.some q7 % q8) q7 (constr q5 q6))
      ||| (y1 === var_ (Nat.succ q9) &&& (y2 === constr q5 q6) &&& (y0 === q10 % q8) &&& _get_termCheck_uni q5 q6 q10 q9 q8 q8)
      ||| (y1 === constr q11 q12 &&& (y2 === var_ Nat.zero) &&& (y0 === Option.some q13 % q8) &&& check_uni (Option.some q13 % q8) (constr q11 q12) q13)
      ||| (y1 === constr q11 q12 &&& (y2 === var_ (Nat.succ q9)) &&& (y0 === q10 % q8) &&& ___get_termCheck_uni q11 q12 q10 q9 q8 q8)
      ||| (y1 === var_ Nat.zero &&& (y2 === var_ q14) &&& (y0 === Option.some q15 % q8) &&& check_uni (Option.some q15 % q8) q15 (var_ q14))
      ||| (y1 === var_ (Nat.succ q9) &&& (y2 === var_ q14) &&& (y0 === q10 % q8) &&& _____get_termCheck_uni q14 q10 q9 q8 q8)
      ||| (y1 === var_ Nat.zero &&& (y2 === var_ Nat.zero) &&& get_termGet_term y0)
      ||| (y1 === var_ (Nat.succ q3) &&& (y2 === var_ (Nat.succ q4)) &&& get_termGet_termEq_nat y0 q3 q4) )
  and forall2 y3 y4 y5 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17)
      ( y4 === List.nil () &&& (y5 === List.nil ())
      ||| (y4 === constr Nat.zero q1 % q2 &&& (y5 === constr Nat.zero q3 % q4) &&& (forall2 y3 q1 q3 &&& forall2 y3 q2 q4))
      ||| (y4 === constr (Nat.succ q5) q1 % q2 &&& (y5 === constr (Nat.succ q6) q3 % q4) &&& (eq_nat q5 q6 &&& forall2 y3 q1 q3 &&& forall2 y3 q2 q4))
      ||| ( y4 === var_ Nat.zero % q2
          &&& (y5 === constr q7 q8 % q4)
          &&& (y3 === Option.some q9 % q10)
          &&& (check_uni (Option.some q9 % q10) q9 (constr q7 q8) &&& forall2 (Option.some q9 % q10) q2 q4) )
      ||| ( y4 === var_ (Nat.succ q11) % q2
          &&& (y5 === constr q7 q8 % q4)
          &&& (y3 === q12 % q10)
          &&& (_get_termCheck_uni q7 q8 q12 q11 q10 q10 &&& forall2 (q12 % q10) q2 q4) )
      ||| ( y4 === constr q13 q14 % q2
          &&& (y5 === var_ Nat.zero % q4)
          &&& (y3 === Option.some q15 % q10)
          &&& (check_uni (Option.some q15 % q10) (constr q13 q14) q15 &&& forall2 (Option.some q15 % q10) q2 q4) )
      ||| ( y4
          === constr q13 q14 % q2
          &&& (y5 === var_ (Nat.succ q11) % q4)
          &&& (y3 === q12 % q10)
          &&& (___get_termCheck_uni q13 q14 q12 q11 q10 q10 &&& forall2 (q12 % q10) q2 q4) )
      ||| ( y4
          === var_ Nat.zero % q2
          &&& (y5 === var_ q16 % q4)
          &&& (y3 === Option.some q17 % q10)
          &&& (check_uni (Option.some q17 % q10) q17 (var_ q16) &&& forall2 (Option.some q17 % q10) q2 q4) )
      ||| ( y4
          === var_ (Nat.succ q11) % q2
          &&& (y5 === var_ q16 % q4)
          &&& (y3 === q12 % q10)
          &&& (_____get_termCheck_uni q16 q12 q11 q10 q10 &&& forall2 (q12 % q10) q2 q4) )
      ||| (y4 === var_ Nat.zero % q2 &&& (y5 === var_ Nat.zero % q4) &&& (get_termGet_term y3 &&& forall2 y3 q2 q4))
      ||| (y4 === var_ (Nat.succ q5) % q2 &&& (y5 === var_ (Nat.succ q6) % q4) &&& (get_termGet_termEq_nat y3 q5 q6 &&& forall2 y3 q2 q4)) )
  and _get_termCheck_uni y12 y13 y15 y16 y17 y18 =
    fresh (q1 q2 q3 q4)
      ( y17
      === Option.some q1 % q2
      &&& (y16 === Nat.zero)
      &&& check_uni (y15 % y18) q1 (constr y12 y13)
      ||| (y17 === q3 % q2 &&& (y16 === Nat.succ q4) &&& _get_termCheck_uni y12 y13 y15 q4 q2 y18) )
  and ___get_termCheck_uni y25 y26 y28 y29 y30 y31 =
    fresh (q1 q2 q3 q4)
      ( y30
      === Option.some q1 % q2
      &&& (y29 === Nat.zero)
      &&& check_uni (y28 % y31) (constr y25 y26) q1
      ||| (y30 === q3 % q2 &&& (y29 === Nat.succ q4) &&& ___get_termCheck_uni y25 y26 y28 q4 q2 y31) )
  and _____get_termCheck_uni y37 y39 y40 y41 y42 =
    fresh (q1 q2 q3 q4)
      ( y41
      === Option.some q1 % q2
      &&& (y40 === Nat.zero)
      &&& check_uni (y39 % y42) q1 (var_ y37)
      ||| (y41 === q3 % q2 &&& (y40 === Nat.succ q4) &&& _____get_termCheck_uni y37 y39 q4 q2 y42) )
  and eq_nat y43 y44 = fresh (q1 q2) (y43 === Nat.zero &&& (y44 === Nat.zero) ||| (y43 === Nat.succ q1 &&& (y44 === Nat.succ q2) &&& eq_nat q1 q2))
  and get_termGet_term y48 =
    fresh (q1) (y48 === List.nil () &&& ___get_term Nat.zero (List.nil ()) ||| (y48 === Option.none () % q1 &&& ___get_term Nat.zero (Option.none () % q1)))
  and get_termGet_termEq_nat y50 y51 y52 =
    fresh (q1 q2) (y50 === List.nil () &&& get_termEq_nat y51 y52 ||| (y50 === q1 % q2 &&& _get_termGet_termEq_nat y52 q1 q2 y51))
  and get_termEq_nat y53 y54 = eq_nat y53 y54
  and _get_termGet_termEq_nat y55 y56 y57 y58 =
    fresh (q1 q2 q3)
      ( y57 === List.nil ()
      &&& _get_termEq_nat y55 y56 (List.nil ()) y58
      ||| (y57 === Option.none () % q1 &&& (y58 === Nat.zero) &&& _get_termEq_nat y55 y56 (Option.none () % q1) Nat.zero)
      ||| (y57 === q2 % q1 &&& (y58 === Nat.succ q3) &&& (_get_termEq_nat y55 y56 (q2 % q1) (Nat.succ q3) &&& ___get_term q3 q1)) )
  and ___get_term y59 y60 =
    fresh (q1 q2 q3)
      (y60 === List.nil () ||| (y60 === Option.none () % q1 &&& (y59 === Nat.zero)) ||| (y60 === q2 % q1 &&& (y59 === Nat.succ q3) &&& ___get_term q3 q1))
  and _get_termEq_nat y61 y62 y63 y64 =
    fresh (q1 q2 q3)
      ( y63 === List.nil () &&& eq_nat y64 y61
      ||| (y63 === Option.none () % q1 &&& (y61 === Nat.zero) &&& eq_nat y64 Nat.zero)
      ||| (y63 === q2 % q1 &&& (y61 === Nat.succ q3) &&& __get_termEq_nat y64 q1 q3) )
  and __get_termEq_nat y65 y66 y67 =
    fresh (q1 q2 q3)
      ( y66 === List.nil ()
      &&& eq_nat y65 (Nat.succ y67)
      ||| (y66 === Option.none () % q1 &&& (y67 === Nat.zero) &&& eq_nat y65 (Nat.succ Nat.zero))
      ||| (y66 === q2 % q1 &&& (y67 === Nat.succ q3) &&& (___get_term q3 q1 &&& eq_nat y65 (Nat.succ (Nat.succ q3)))) )
  in
  check_uni x0 x1 x2