open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = let rec _evalo y2 y3 = (fresh (q1 q2 q3 q4 q5 q6) ((((y2 === conj q1 q2) &&& (evaloEvalo y3 q1 q2)) ||| (((y2 === disj q1 q2) &&& ((evaloEvalo y3 q1 q2) ||| (((__evalo y3 q1) &&& (_evalo q2 y3)) ||| (___evaloEvalo y3 q1 q2)))) ||| (((y2 === neg q1) &&& (__evalo y3 q1)) ||| ((y2 === var q3) &&& (((q3 === Std.Nat.zero) &&& (y3 === (Std.(%) (!!true) (q4)))) ||| (((q3 === (Std.Nat.succ (q5))) &&& (y3 === (Std.(%) (q6) (q4)))) &&& (_elemo q4 q5)))))))))
and evaloEvalo y4 y5 y6 = ((_evalo y5 y4) &&& (_evalo y6 y4))
and __evalo y7 y8 = (fresh (q1 q2 q3 q4 q5 q6) ((((y8 === conj q1 q2) &&& ((_evaloEvalo y7 q1 q2) ||| (((_evalo q1 y7) &&& (__evalo y7 q2)) ||| (__evaloEvalo y7 q1 q2)))) ||| (((y8 === disj q1 q2) &&& (__evaloEvalo y7 q1 q2)) ||| (((y8 === neg q1) &&& (_evalo q1 y7)) ||| ((y8 === var q3) &&& (((q3 === Std.Nat.zero) &&& (y7 === (Std.(%) (!!false) (q4)))) ||| (((q3 === (Std.Nat.succ (q5))) &&& (y7 === (Std.(%) (q6) (q4)))) &&& (elemo q4 q5)))))))))
and _evaloEvalo y9 y10 y11 = ((__evalo y9 y10) &&& (_evalo y11 y9))
and __evaloEvalo y12 y13 y14 = ((__evalo y12 y13) &&& (__evalo y12 y14))
and elemo y15 y16 = (fresh (q1 q2 q3) ((((y16 === Std.Nat.zero) &&& (y15 === (Std.(%) (!!false) (q1)))) ||| (((y16 === (Std.Nat.succ (q2))) &&& (y15 === (Std.(%) (q3) (q1)))) &&& (elemo q1 q2)))))
and ___evaloEvalo y17 y18 y19 = ((_evalo y18 y17) &&& (__evalo y17 y19))
and _elemo y20 y21 = (fresh (q1 q2 q3) ((((y21 === Std.Nat.zero) &&& (y20 === (Std.(%) (!!true) (q1)))) ||| (((y21 === (Std.Nat.succ (q2))) &&& (y20 === (Std.(%) (q3) (q1)))) &&& (_elemo q1 q2))))) in         (_evalo x1 x0)
