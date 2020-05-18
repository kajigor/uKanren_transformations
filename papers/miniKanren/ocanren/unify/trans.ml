open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec check_uni y0 y1 y2 =
    fresh
      (x48 x47 x46 x24 x23 x22 x21 x20 x19 x18 x17 x16 x15 x45 x44 x43 x14 x13 x12 x11 x10 x42 x41 x40 x39 x38 x37 x36 x35 x34 x31 x30 x29 x26 x9 x8 x7 x6 x5
         x4 x3)
      ( ( x3 === Std.Pair.pair y1 y2
          &&& (y1 === constr x4 x5)
          &&& (y2 === constr x6 x7)
          &&& (x8 === !!true) &&& (x9 === !!true)
          &&& ( ( x26 === Std.Pair.pair x4 x6 &&& (x4 === Std.Nat.zero) &&& (x6 === Std.Nat.zero)
                  ||| ( x26 === Std.Pair.pair x4 x6
                      &&& (x4 === Std.Nat.succ x29)
                      &&& (x6 === Std.Nat.succ x30)
                      &&& ( x31 === Std.Pair.pair x29 x30 &&& (x29 === Std.Nat.zero) &&& (x30 === Std.Nat.zero)
                          ||| (x31 === Std.Pair.pair x29 x30 &&& (x29 === Std.Nat.succ x34) &&& (x30 === Std.Nat.succ x35) &&& eq_nat x34 x35) ) ) )
              &&& (( x36 === Std.Pair.pair x5 x7
                      &&& (x5 === Std.List.nil ())
                      &&& (x7 === Std.List.nil ())
                      ||| ( x36 === Std.Pair.pair x5 x7
                          &&& (x5 === Std.( % ) x37 x38)
                          &&& (x7 === Std.( % ) x39 x40)
                          &&& (x41 === !!true) &&& (x42 === !!true)
                          &&& (_check_uni y0 x37 x39 &&& (forall2 y0 x38 x40)) ) ) ) )
          ||| ( x3 === Std.Pair.pair y1 y2
              &&& (y1 === var_ x10)
              &&& (y2 === constr x11 x12)
              &&& (x13 === Std.Option.some x14)
              &&& ( ( y0 === Std.( % ) x43 x44 &&& (x10 === Std.Nat.zero)
                      &&& (x43 === Std.Option.some x14)
                      ||| (y0 === Std.( % ) x43 x44 &&& (x10 === Std.Nat.succ x45) &&& get_term x44 x45 x14) )
                  &&& ( _check_uni y0 x14 (constr x11 x12)) ) )
          ||| ( x3 === Std.Pair.pair y1 y2
              &&& (y1 === constr x15 x16)
              &&& (y2 === var_ x17)
              &&& (x18 === Std.Option.some x19)
              &&& ( get_term y0 x17 x19 &&& ( _check_uni y0 (constr x15 x16) x19)) )
          ||| ( x3 === Std.Pair.pair y1 y2
              &&& (y1 === var_ x20)
              &&& (y2 === var_ x21)
              &&& (x22 === Std.Option.some x23)
              &&& (get_term y0 x20 x23 &&& ( _check_uni y0 x23 (var_ x21))) )
          ||| ( x3 === Std.Pair.pair y1 y2
              &&& (y1 === var_ x20)
              &&& (y2 === var_ x21)
              &&& (x22 === Std.Option.none ())
              &&& (x24 === Std.Option.none ())
              &&& (  ( ( y0 === Std.List.nil ()
                          ||| (y0 === Std.( % ) x46 x47 &&& (x20 === Std.Nat.zero) &&& (x46 === Std.Option.none ()))
                          ||| (y0 === Std.( % ) x46 x47 &&& (x20 === Std.Nat.succ x48) &&& _get_term x47 x48) )
                      &&& ( _get_term y0 x21) )
                  &&& ( eq_nat x20 x21) ) ) ) )
  and eq_nat y3 y4 =
    fresh (x35 x34 x31 x6 x4 x26)
      ( x26 === Std.Pair.pair x4 x6
      &&& (x4 === Std.Nat.succ y3)
      &&& (x6 === Std.Nat.succ y4)
      &&& ( x31 === Std.Pair.pair y3 y4 &&& (y3 === Std.Nat.zero) &&& (y4 === Std.Nat.zero)
          ||| (x31 === Std.Pair.pair y3 y4 &&& (y3 === Std.Nat.succ x34) &&& (y4 === Std.Nat.succ x35) &&& eq_nat x34 x35) ) )
  and _check_uni y5 y6 y7 =
    fresh
      (x48 x47 x46 x24 x23 x22 x21 x20 x19 x18 x17 x16 x15 x45 x44 x43 x14 x13 x12 x11 x10 x42 x41 x40 x39 x38 x37 x36 x35 x34 x31 x30 x29 x26 x9 x8 x7 x6 x5
         x4 x3)
      ( ( x3 === Std.Pair.pair y6 y7
          &&& (y6 === constr x4 x5)
          &&& (y7 === constr x6 x7)
          &&& (x8 === !!true) &&& (x9 === !!true)
          &&& (  ( x26 === Std.Pair.pair x4 x6 &&& (x4 === Std.Nat.zero) &&& (x6 === Std.Nat.zero)
                  ||| ( x26 === Std.Pair.pair x4 x6
                      &&& (x4 === Std.Nat.succ x29)
                      &&& (x6 === Std.Nat.succ x30)
                      &&& ( x31 === Std.Pair.pair x29 x30 &&& (x29 === Std.Nat.zero) &&& (x30 === Std.Nat.zero)
                          ||| (x31 === Std.Pair.pair x29 x30 &&& (x29 === Std.Nat.succ x34) &&& (x30 === Std.Nat.succ x35) &&& eq_nat x34 x35) ) ) )
              &&& ( ( x36 === Std.Pair.pair x5 x7
                      &&& (x5 === Std.List.nil ())
                      &&& (x7 === Std.List.nil ())
                      ||| ( x36 === Std.Pair.pair x5 x7
                          &&& (x5 === Std.( % ) x37 x38)
                          &&& (x7 === Std.( % ) x39 x40)
                          &&& (x41 === !!true) &&& (x42 === !!true)
                          &&& ( _check_uni y5 x37 x39 &&& (forall2 y5 x38 x40)) ) ) ) )
          ||| ( x3 === Std.Pair.pair y6 y7
              &&& (y6 === var_ x10)
              &&& (y7 === constr x11 x12)
              &&& (x13 === Std.Option.some x14)
              &&& ( ( y5 === Std.( % ) x43 x44 &&& (x10 === Std.Nat.zero)
                      &&& (x43 === Std.Option.some x14)
                      ||| (y5 === Std.( % ) x43 x44 &&& (x10 === Std.Nat.succ x45) &&& get_term x44 x45 x14) )
                  &&& (_check_uni y5 x14 (constr x11 x12)) ) )
          ||| ( x3 === Std.Pair.pair y6 y7
              &&& (y6 === constr x15 x16)
              &&& (y7 === var_ x17)
              &&& (x18 === Std.Option.some x19)
              &&& (get_term y5 x17 x19 &&& (_check_uni y5 (constr x15 x16) x19)) )
          ||| ( x3 === Std.Pair.pair y6 y7
              &&& (y6 === var_ x20)
              &&& (y7 === var_ x21)
              &&& (x22 === Std.Option.some x23)
              &&& ( get_term y5 x20 x23 &&& (_check_uni y5 x23 (var_ x21))) )
          ||| ( x3 === Std.Pair.pair y6 y7
              &&& (y6 === var_ x20)
              &&& (y7 === var_ x21)
              &&& (x22 === Std.Option.none ())
              &&& (x24 === Std.Option.none ())
              &&& ((  ( y5 === Std.List.nil ()
                          ||| (y5 === Std.( % ) x46 x47 &&& (x20 === Std.Nat.zero) &&& (x46 === Std.Option.none ()))
                          ||| (y5 === Std.( % ) x46 x47 &&& (x20 === Std.Nat.succ x48) &&& _get_term x47 x48) )
                      &&& ( _get_term y5 x21) )
                  &&& ( eq_nat x20 x21) ) ) ) )
  and forall2 y8 y9 y10 =
    fresh (x42 x41 x40 x39 x38 x37 x36)
      ( ( x36 === Std.Pair.pair y9 y10
          &&& (y9 === Std.List.nil ())
          &&& (y10 === Std.List.nil ())
          ||| ( x36 === Std.Pair.pair y9 y10
              &&& (y9 === Std.( % ) x37 x38)
              &&& (y10 === Std.( % ) x39 x40)
              &&& (x41 === !!true) &&& (x42 === !!true)
              &&& (_check_uni y8 x37 x39 &&& (forall2 y8 x38 x40)) ) ) )
  and get_term y11 y12 y13 =
    fresh (x45 x44 x43)
      (( y11 === Std.( % ) x43 x44 &&& (y12 === Std.Nat.zero)
          &&& (x43 === Std.Option.some y13)
          ||| (y11 === Std.( % ) x43 x44 &&& (y12 === Std.Nat.succ x45) &&& get_term x44 x45 y13) ) )
  and _get_term y14 y15 =
    fresh (x48 x47 x46)
      (  ( y14 === Std.List.nil ()
          ||| (y14 === Std.( % ) x46 x47 &&& (y15 === Std.Nat.zero) &&& (x46 === Std.Option.none ()))
          ||| (y14 === Std.( % ) x46 x47 &&& (y15 === Std.Nat.succ x48) &&& _get_term x47 x48) ) )
  in
  _check_uni x0 x1 x2