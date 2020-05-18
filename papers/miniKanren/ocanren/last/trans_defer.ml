open OCanren
open GT
open Helper

(* extra defers *)
let topLevel x0 x1 =
  let rec _evalo y2 y3 =
    fresh (q1 q2 q3 q4 q5 q6)
      (defer
         ( defer
             ( defer
                 ( defer (y3 === conj q1 q2 &&& defer (___evaloEvalo y2 q1 q2))
                 ||| defer
                       ( y3 === disj q1 q2
                       &&& defer
                             ( defer (defer (defer (__evalo y2 q1) &&& defer (_evalo y2 q2)) ||| defer (__evaloEvalo y2 q1 q2))
                             ||| defer (___evaloEvalo y2 q1 q2) ) ) )
             ||| defer (y3 === neg q1 &&& defer (__evalo y2 q1)) )
         ||| defer
               ( y3 === var q3
               &&& defer
                     ( defer (q3 === Std.Nat.zero &&& (y2 === Std.( % ) !!true q4))
                     ||| defer (defer (q3 === Std.Nat.succ q5 &&& (y2 === Std.( % ) q6 q4)) &&& defer (_elemo q4 q5)) ) ) ))
  and __evalo y4 y5 =
    fresh (q1 q2 q3 q4 q5 q6)
      (defer
         ( defer
             ( defer
                 ( defer
                     ( y5 === conj q1 q2
                     &&& defer (defer (defer (_evaloEvalo y4 q1 q2) ||| defer (evaloEvalo y4 q1 q2)) ||| defer (defer (_evalo y4 q1) &&& defer (__evalo y4 q2)))
                     )
                 ||| defer (y5 === disj q1 q2 &&& defer (_evaloEvalo y4 q1 q2)) )
             ||| defer (y5 === neg q1 &&& defer (_evalo y4 q1)) )
         ||| defer
               ( y5 === var q3
               &&& defer
                     ( defer (q3 === Std.Nat.zero &&& (y4 === Std.( % ) !!false q4))
                     ||| defer (defer (q3 === Std.Nat.succ q5 &&& (y4 === Std.( % ) q6 q4)) &&& defer (elemo q4 q5)) ) ) ))
  and evaloEvalo y6 y7 y8 = defer (defer (__evalo y6 y7) &&& defer (_evalo y6 y8))
  and _evaloEvalo y9 y10 y11 = defer (defer (__evalo y9 y10) &&& defer (__evalo y9 y11))
  and elemo y12 y13 =
    fresh (q1 q2 q3)
      (defer
         ( defer (y13 === Std.Nat.zero &&& (y12 === Std.( % ) !!false q1))
         ||| defer (defer (y13 === Std.Nat.succ q2 &&& (y12 === Std.( % ) q3 q1)) &&& defer (elemo q1 q2)) ))
  and __evaloEvalo y14 y15 y16 = defer (defer (_evalo y14 y15) &&& defer (__evalo y14 y16))
  and ___evaloEvalo y17 y18 y19 = defer (defer (_evalo y17 y18) &&& defer (_evalo y17 y19))
  and _elemo y20 y21 =
    fresh (q1 q2 q3)
      (defer
         ( defer (y21 === Std.Nat.zero &&& (y20 === Std.( % ) !!true q1))
         ||| defer (defer (y21 === Std.Nat.succ q2 &&& (y20 === Std.( % ) q3 q1)) &&& defer (_elemo q1 q2)) ))
  in
  defer (_evalo x0 x1)