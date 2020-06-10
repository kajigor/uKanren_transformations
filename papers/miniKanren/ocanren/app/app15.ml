open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec appendo y0 y1 y2 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40
         q41 q42 q43 q44 q45 q46 q47 q48)
      ( y0 === Std.List.nil () &&& (y1 === y2)
      ||| ( y0 === Std.( % ) q1 q2
          &&& (y2 === Std.( % ) q1 q3)
          &&& ( q2 === Std.List.nil () &&& (y1 === q3)
              ||| ( q2 === Std.( % ) q4 q5
                  &&& (q3 === Std.( % ) q4 q6)
                  &&& ( q5 === Std.List.nil () &&& (y1 === q6)
                      ||| ( q5 === Std.( % ) q7 q8
                          &&& (q6 === Std.( % ) q7 q9)
                          &&& ( q8 === Std.List.nil () &&& (y1 === q9)
                              ||| ( q8 === Std.( % ) q10 q11
                                  &&& (q9 === Std.( % ) q10 q12)
                                  &&& ( q11 === Std.List.nil () &&& (y1 === q12)
                                      ||| ( q11 === Std.( % ) q13 q14
                                          &&& (q12 === Std.( % ) q13 q15)
                                          &&& ( q14 === Std.List.nil () &&& (y1 === q15)
                                              ||| ( q14 === Std.( % ) q16 q17
                                                  &&& (q15 === Std.( % ) q16 q18)
                                                  &&& ( q17 === Std.List.nil () &&& (y1 === q18)
                                                      ||| ( q17 === Std.( % ) q19 q20
                                                          &&& (q18 === Std.( % ) q19 q21)
                                                          &&& ( q20 === Std.List.nil () &&& (y1 === q21)
                                                              ||| ( q20 === Std.( % ) q22 q23
                                                                  &&& (q21 === Std.( % ) q22 q24)
                                                                  &&& ( q23 === Std.List.nil () &&& (y1 === q24)
                                                                      ||| ( q23 === Std.( % ) q25 q26
                                                                          &&& (q24 === Std.( % ) q25 q27)
                                                                          &&& ( q26 === Std.List.nil () &&& (y1 === q27)
                                                                              ||| ( q26 === Std.( % ) q28 q29
                                                                                  &&& (q27 === Std.( % ) q28 q30)
                                                                                  &&& ( q29 === Std.List.nil () &&& (y1 === q30)
                                                                                      ||| ( q29 === Std.( % ) q31 q32
                                                                                          &&& (q30 === Std.( % ) q31 q33)
                                                                                          &&& ( q32 === Std.List.nil () &&& (y1 === q33)
                                                                                              ||| ( q32 === Std.( % ) q34 q35
                                                                                                  &&& (q33 === Std.( % ) q34 q36)
                                                                                                  &&& ( q35 === Std.List.nil () &&& (y1 === q36)
                                                                                                      ||| ( q35 === Std.( % ) q37 q38
                                                                                                          &&& (q36 === Std.( % ) q37 q39)
                                                                                                          &&& ( q38 === Std.List.nil () &&& (y1 === q39)
                                                                                                              ||| ( q38 === Std.( % ) q40 q41
                                                                                                                  &&& (q39 === Std.( % ) q40 q42)
                                                                                                                  &&& ( q41 === Std.List.nil () &&& (y1 === q42)
                                                                                                                      ||| ( q41 === Std.( % ) q43 q44
                                                                                                                          &&& (q42 === Std.( % ) q43 q45)
                                                                                                                          &&& ( q44 === Std.List.nil ()
                                                                                                                              &&& (y1 === q45)
                                                                                                                              ||| ( q44 === Std.( % ) q46 q47
                                                                                                                                  &&& ( q45
                                                                                                                                      === Std.( % ) q46 q48 )
                                                                                                                                  &&& _appendo y1 q47 q48 ) )
                                                                                                                          ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
                                                  ) ) ) ) ) ) ) ) ) ) ) )
  and _appendo y3 y4 y5 =
    fresh (q1 q2 q3) (y4 === Std.List.nil () &&& (y3 === y5) ||| (y4 === Std.( % ) q1 q2 &&& (y5 === Std.( % ) q1 q3) &&& _appendo y3 q2 q3))
  in
  appendo x0 x1 x2
