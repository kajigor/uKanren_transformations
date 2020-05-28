open GT
open OCanren
open OCanren.Std

let topLevel x0 x1 =
  let rec rep y0 y1 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40
         q41 q42)
      ( y0 === Std.Nat.zero
      &&& (y1 === Std.List.nil ())
      ||| ( y0 === Std.Nat.succ q1
          &&& (y1 === Std.( % ) Std.Nat.zero q2)
          &&& ( q1 === Std.Nat.zero
              &&& (q2 === Std.List.nil ())
              ||| ( q1 === Std.Nat.succ q3
                  &&& (q2 === Std.( % ) Std.Nat.zero q4)
                  &&& ( q3 === Std.Nat.zero
                      &&& (q4 === Std.List.nil ())
                      ||| ( q3 === Std.Nat.succ q5
                          &&& (q4 === Std.( % ) Std.Nat.zero q6)
                          &&& ( q5 === Std.Nat.zero
                              &&& (q6 === Std.List.nil ())
                              ||| ( q5 === Std.Nat.succ q7
                                  &&& (q6 === Std.( % ) Std.Nat.zero q8)
                                  &&& ( q7 === Std.Nat.zero
                                      &&& (q8 === Std.List.nil ())
                                      ||| ( q7 === Std.Nat.succ q9
                                          &&& (q8 === Std.( % ) Std.Nat.zero q10)
                                          &&& ( q9 === Std.Nat.zero
                                              &&& (q10 === Std.List.nil ())
                                              ||| ( q9 === Std.Nat.succ q11
                                                  &&& (q10 === Std.( % ) Std.Nat.zero q12)
                                                  &&& ( q11 === Std.Nat.zero
                                                      &&& (q12 === Std.List.nil ())
                                                      ||| ( q11 === Std.Nat.succ q13
                                                          &&& (q12 === Std.( % ) Std.Nat.zero q14)
                                                          &&& ( q13 === Std.Nat.zero
                                                              &&& (q14 === Std.List.nil ())
                                                              ||| ( q13 === Std.Nat.succ q15
                                                                  &&& (q14 === Std.( % ) Std.Nat.zero q16)
                                                                  &&& ( q15 === Std.Nat.zero
                                                                      &&& (q16 === Std.List.nil ())
                                                                      ||| ( q15 === Std.Nat.succ q17
                                                                          &&& (q16 === Std.( % ) Std.Nat.zero q18)
                                                                          &&& ( q17 === Std.Nat.zero
                                                                              &&& (q18 === Std.List.nil ())
                                                                              ||| ( q17 === Std.Nat.succ q19
                                                                                  &&& (q18 === Std.( % ) Std.Nat.zero q20)
                                                                                  &&& ( q19 === Std.Nat.zero
                                                                                      &&& (q20 === Std.List.nil ())
                                                                                      ||| ( q19 === Std.Nat.succ q21
                                                                                          &&& (q20 === Std.( % ) Std.Nat.zero q22)
                                                                                          &&& ( q21 === Std.Nat.zero
                                                                                              &&& (q22 === Std.List.nil ())
                                                                                              ||| ( q21 === Std.Nat.succ q23
                                                                                                  &&& (q22 === Std.( % ) Std.Nat.zero q24)
                                                                                                  &&& ( q23 === Std.Nat.zero
                                                                                                      &&& (q24 === Std.List.nil ())
                                                                                                      ||| ( q23 === Std.Nat.succ q25
                                                                                                          &&& (q24 === Std.( % ) Std.Nat.zero q26)
                                                                                                          &&& ( q25 === Std.Nat.zero
                                                                                                              &&& (q26 === Std.List.nil ())
                                                                                                              ||| ( q25 === Std.Nat.succ q27
                                                                                                                  &&& (q26 === Std.( % ) Std.Nat.zero q28)
                                                                                                                  &&& ( q27 === Std.Nat.zero
                                                                                                                      &&& (q28 === Std.List.nil ())
                                                                                                                      ||| ( q27 === Std.Nat.succ q29
                                                                                                                          &&& ( q28
                                                                                                                              === Std.( % ) Std.Nat.zero q30 )
                                                                                                                          &&& ( q29 === Std.Nat.zero
                                                                                                                              &&& (q30 === Std.List.nil ())
                                                                                                                              ||| ( q29 === Std.Nat.succ q31
                                                                                                                                  &&& ( q30
                                                                                                                                      === Std.( % )
                                                                                                                                            Std.Nat.zero q32 )
                                                                                                                                  &&& ( q31 === Std.Nat.zero
                                                                                                                                      &&& ( q32
                                                                                                                                          === Std.List.nil ()
                                                                                                                                          )
                                                                                                                                      ||| ( q31
                                                                                                                                          === Std.Nat.succ q33
                                                                                                                                          &&& ( q32
                                                                                                                                              === Std.( % )
                                                                                                                                                    Std.Nat
                                                                                                                                                    .zero q34
                                                                                                                                              )
                                                                                                                                          &&& ( q33
                                                                                                                                              === Std.Nat.zero
                                                                                                                                              &&& ( q34
                                                                                                                                                  === Std.List
                                                                                                                                                      .nil ()
                                                                                                                                                  )
                                                                                                                                              ||| ( q33
                                                                                                                                                  === Std.Nat
                                                                                                                                                      .succ q35
                                                                                                                                                  &&& ( q34
                                                                                                                                                      === Std
                                                                                                                                                          .( % )
                                                                                                                                                            Std
                                                                                                                                                            .Nat
                                                                                                                                                            .zero
                                                                                                                                                            q36
                                                                                                                                                      )
                                                                                                                                                  &&& ( q35
                                                                                                                                                      === Std
                                                                                                                                                          .Nat
                                                                                                                                                          .zero
                                                                                                                                                      &&& ( q36
                                                                                                                                                          === Std
                                                                                                                                                              .List
                                                                                                                                                              .nil
                                                                                                                                                               ()
                                                                                                                                                          )
                                                                                                                                                      ||| ( q35
                                                                                                                                                          === Std
                                                                                                                                                              .Nat
                                                                                                                                                              .succ
                                                                                                                                                               q37
                                                                                                                                                          &&& ( 
                                                                                                                                                              q36
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .( % )
                                                                                                                                                              Std
                                                                                                                                                              .Nat
                                                                                                                                                              .zero
                                                                                                                                                              q38
                                                                                                                                                              )
                                                                                                                                                          &&& ( 
                                                                                                                                                              q37
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .Nat
                                                                                                                                                            .zero
                                                                                                                                                            &&& 
                                                                                                                                                            ( q38
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .List
                                                                                                                                                            .nil
                                                                                                                                                              ()
                                                                                                                                                            )
                                                                                                                                                              ||| 
                                                                                                                                                              ( 
                                                                                                                                                              q37
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .Nat
                                                                                                                                                            .succ
                                                                                                                                                              q39
                                                                                                                                                            &&& 
                                                                                                                                                            ( q38
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .( % )
                                                                                                                                                              Std
                                                                                                                                                              .Nat
                                                                                                                                                              .zero
                                                                                                                                                              q40
                                                                                                                                                            )
                                                                                                                                                              &&& 
                                                                                                                                                              ( 
                                                                                                                                                              q39
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .Nat
                                                                                                                                                            .zero
                                                                                                                                                            &&& 
                                                                                                                                                            ( q40
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .List
                                                                                                                                                            .nil
                                                                                                                                                              ()
                                                                                                                                                            )
                                                                                                                                                              ||| 
                                                                                                                                                              ( 
                                                                                                                                                              q39
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .Nat
                                                                                                                                                            .succ
                                                                                                                                                              q41
                                                                                                                                                            &&& 
                                                                                                                                                            ( q40
                                                                                                                                                            === 
                                                                                                                                                            Std
                                                                                                                                                            .( % )
                                                                                                                                                              Std
                                                                                                                                                              .Nat
                                                                                                                                                              .zero
                                                                                                                                                              q42
                                                                                                                                                            )
                                                                                                                                                              &&& 
                                                                                                                                                              _rep
                                                                                                                                                               q41
                                                                                                                                                               q42
                                                                                                                                                              )
                                                                                                                                                              )
                                                                                                                                                              )
                                                                                                                                                              )
                                                                                                                                                          ) )
                                                                                                                                                  ) ) ) ) ) )
                                                                                                                          ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
                                                  ) ) ) ) ) ) ) ) ) ) ) )
  and _rep y2 y3 =
    fresh (q1 q2) (y2 === Std.Nat.zero &&& (y3 === Std.List.nil ()) ||| (y2 === Std.Nat.succ q1 &&& (y3 === Std.( % ) Std.Nat.zero q2) &&& _rep q1 q2))
  in
  rep x0 x1
