open OCanren
open OCanren.Std

type 'a0 gnat = Z | S of 'a0

let rec fmap fa0 = function Z -> Z | S a0 -> S (fa0 a0)

module For_gnat = Fmap (struct
  let rec fmap fa0 = function Z -> Z | S a0 -> S (fa0 a0)

  type 'a0 t = 'a0 gnat
end)

let rec z () = inj (For_gnat.distrib Z)

and s x__0 = inj (For_gnat.distrib (S x__0))

type ('a1, 'a0) gterm = Var_ of 'a1 | Constr of 'a1 * 'a0

let rec fmap fa1 fa0 = function Var_ a1 -> Var_ (fa1 a1) | Constr (a1_0, a0_1) -> Constr (fa1 a1_0, fa0 a0_1)

module For_gterm = Fmap2 (struct
  let rec fmap fa1 fa0 = function Var_ a1 -> Var_ (fa1 a1) | Constr (a1_0, a0_1) -> Constr (fa1 a1_0, fa0 a0_1)

  type ('a1, 'a0) t = ('a1, 'a0) gterm
end)

let rec var_ x__0 = inj (For_gterm.distrib (Var_ x__0))

and constr x__0 x__1 = inj (For_gterm.distrib (Constr (x__0, x__1)))

let topLevel x0 x1 x2 =
  let rec check_uni y0 y1 y2 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12)
      ( y1 === constr q1 q2
      &&& (y2 === constr q3 q4)
      &&& (eq_nat q1 q3 &&& forall2 y0 q2 q4)
      ||| (y1 === var_ q5 &&& (y2 === constr q6 q7) &&& get_termCheck_uni y0 q5 q6 q7)
      ||| (y1 === constr q8 q9 &&& (y2 === var_ q10) &&& _get_termCheck_uni y0 q8 q9 q10)
      ||| (y1 === var_ q11 &&& (y2 === var_ q12) &&& __get_termCheck_uni y0 q11 q12)
      ||| (y1 === var_ q11 &&& (y2 === var_ q12) &&& get_termGet_termEq_nat y0 q11 q12) )
  and eq_nat y3 y4 = fresh (q1 q2) (y3 === z () &&& (y4 === z ()) ||| (y3 === s q1 &&& (y4 === s q2) &&& eq_nat q1 q2))
  and forall2 y5 y6 y7 =
    fresh (q1 q2 q3 q4) (y6 === nil () &&& (y7 === nil ()) ||| (y6 === q1 % q2 &&& (y7 === q3 % q4) &&& (check_uni y5 q1 q3 &&& forall2 y5 q2 q4)))
  and get_termCheck_uni y8 y9 y10 y11 =
    fresh (q1 q2 q3 q4)
      ( y8
      === some q1 % q2
      &&& (y9 === z ())
      &&& check_uni (some q1 % q2) q1 (constr y10 y11)
      ||| (y8 === q3 % q2 &&& (y9 === s q4) &&& (__get_term q1 q2 q4 &&& check_uni (q3 % q2) q1 (constr y10 y11))) )
  and _get_termCheck_uni y16 y17 y18 y19 =
    fresh (q1 q2 q3 q4)
      ( y16
      === some q1 % q2
      &&& (y19 === z ())
      &&& check_uni (some q1 % q2) (constr y17 y18) q1
      ||| (y16 === q3 % q2 &&& (y19 === s q4) &&& (__get_term q1 q2 q4 &&& check_uni (q3 % q2) (constr y17 y18) q1)) )
  and __get_termCheck_uni y24 y25 y26 =
    fresh (q1 q2 q3 q4)
      ( y24
      === some q1 % q2
      &&& (y25 === z ())
      &&& check_uni (some q1 % q2) q1 (var_ y26)
      ||| (y24 === q3 % q2 &&& (y25 === s q4) &&& (__get_term q1 q2 q4 &&& check_uni (q3 % q2) q1 (var_ y26))) )
  and __get_term y28 y29 y30 = fresh (q1 q2 q3) (y29 === some y28 % q1 &&& (y30 === z ()) ||| (y29 === q2 % q1 &&& (y30 === s q3) &&& __get_term y28 q1 q3))
  and get_termGet_termEq_nat y31 y32 y33 =
    fresh (q1 q2 q3 q4)
      ( y31 === nil () &&& eq_nat y32 y33
      ||| (y31 === none ()  % q1 &&& (y32 === z ()) &&& (y33 === z ()))
      ||| (y31 === q2 % q1 &&& (y32 === s q3) &&& (y33 === s q4) &&& get_termGet_termEq_nat q1 q3 q4) )
  in
  check_uni x0 x1 x2
