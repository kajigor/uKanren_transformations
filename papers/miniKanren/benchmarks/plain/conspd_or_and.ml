open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 =
  let rec _evalo y2 y3 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y2 === conj q1 q2 &&& evaloEvalo y3 q1 q2
      ||| ( y2 === disj q1 q2
          &&& ( orEvaloEvalo y3 q1 q2
              )
          ||| ( y2 === neg q1 &&& __evalo y3 q1
              ||| ( y2 === var q3
                  &&& ( _elemo y3 q3  ) ) ) ) )
  and evaloEvalo y4 y5 y6 = _evalo y5 y4 &&& _evalo y6 y4
  and __evalo y7 y8 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y8 === conj q1 q2
      &&& ( andEvaloEvalo y7 q1 q2)
      ||| ( y8 === disj q1 q2 &&& __evaloEvalo y7 q1 q2
          ||| ( y8 === neg q1 &&& _evalo q1 y7
              ||| ( y8 === var q3
                  &&& ( elemo y7 q3 ) ) ) ) )
  and _evaloEvalo y9 y10 y11 = __evalo y9 y10 &&& _evalo y11 y9
  and __evaloEvalo y12 y13 y14 = __evalo y12 y13 &&& __evalo y12 y14
  and orEvaloEvalo subst x y =
    _evalo x subst &&& _evalo y subst
              ||| (__evalo subst x &&& _evalo y subst ||| _evalo x subst &&& __evalo subst y)
  and andEvaloEvalo subst x y =
    __evalo subst x &&& _evalo y subst
    ||| (_evalo x subst &&& __evalo subst y ||| __evalo subst x &&& __evalo subst y)
  and elemo y15 y16 =
    fresh (q1 q2 q3)
      ( y16 === Std.Nat.zero
      &&& (y15 === Std.( % ) !!false q1)
      ||| ( y16 === Std.Nat.succ q2
          &&& (y15 === Std.( % ) q3 q1)
          &&& elemo q1 q2 ) )
  and ___evaloEvalo y17 y18 y19 = _evalo y18 y17 &&& __evalo y17 y19
  and _elemo y20 y21 =
    fresh (q1 q2 q3)
      ( y21 === Std.Nat.zero
      &&& (y20 === Std.( % ) !!true q1)
      ||| ( y21 === Std.Nat.succ q2
          &&& (y20 === Std.( % ) q3 q1)
          &&& _elemo q1 q2 ) )
  in
  _evalo x1 x0
(* open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 =
  let rec _evalo y2 y3 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y2 === conj q1 q2 &&& evaloEvalo y3 q1 q2
      ||| ( y2 === disj q1 q2 &&& orEvaloEvalo y3 q1 q2
          ||| ( y2 === neg q1 &&& __evalo y3 q1
              ||| (y2 === var q3 &&& _elemo y3 q3) ) ) )
  and evaloEvalo y4 y5 y6 = _evalo y5 y4 &&& _evalo y6 y4
  and __evalo y7 y8 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y8 === conj q1 q2 &&& andEvaloEvalo y7 q1 q2
      ||| ( y8 === disj q1 q2 &&& __evaloEvalo y7 q1 q2
          ||| ( y8 === neg q1 &&& _evalo q1 y7
              ||| (y8 === var q3 &&& elemo y7 q3) ) ) )
  and _evaloEvalo y9 y10 y11 = __evalo y9 y10 &&& _evalo y11 y9
  and __evaloEvalo y12 y13 y14 = __evalo y12 y13 &&& __evalo y12 y14
  and orEvaloEvalo subst x y =
    _evalo x subst &&& _evalo y subst
    ||| ( __evalo subst x &&& _evalo y subst ||| _evalo x subst
        &&& __evalo subst y )
  and andEvaloEvalo subst x y =
    __evalo subst x &&& _evalo y subst
    ||| ( _evalo x subst &&& __evalo subst y ||| __evalo subst x
        &&& __evalo subst y )
  and elemo y15 y16 =
    fresh (q1 q2 q3)
      ( y16 === Std.Nat.zero
      &&& (y15 === Std.( % ) !!false q1)
      ||| ( y16 === Std.Nat.succ q2
          &&& (y15 === Std.( % ) q3 q1)
          &&& elemo q1 q2 ) )
  and ___evaloEvalo y17 y18 y19 = _evalo y18 y17 &&& __evalo y17 y19
  and _elemo y20 y21 =
    fresh (q1 q2 q3)
      ( y21 === Std.Nat.zero
      &&& (y20 === Std.( % ) !!true q1)
      ||| ( y21 === Std.Nat.succ q2
          &&& (y20 === Std.( % ) q3 q1)
          &&& _elemo q1 q2 ) )
  in
  _evalo x1 x0 *)
