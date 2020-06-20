open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 =
  let rec checkAnswer y0 y1 y2 y3 = fresh q1 (startState y1 y2 q1 &&& calcFuel y0 y1 y2 y3 q1)
  and startState y4 y5 y6 =
    fresh (q1 q2 q3)
      (y6 === st (o ()) y5 q1 &&& (y4 === o () &&& (q1 === Std.List.nil ()) ||| (y4 === s q2 &&& (q1 === Std.( % ) (o ()) q3) &&& stations q2 q3)))
  and calcFuel y7 y8 y9 y10 y11 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)
      ( y7 === Std.List.nil () &&& (y9 === y10)
      &&& (y11 === st (o ()) q1 q2 &&& (y8 === o ()) ||| (y11 === st (s q3) q1 q2 &&& (y8 === s q4) &&& eqNat q4 q3))
      ||| ( y7 === Std.( % ) q5 q6
          &&& ( q5 === left q7
              &&& (checkStep y8 y9 y11 q7 &&& stepCalcFuel y8 y9 y11 q6 y10 q7)
              ||| (q5 === right q8 &&& (checkStepStepCalcFuel y8 y9 y11 q6 q9 q8 &&& _getFuel y9 y11 q10 q8 &&& add q9 y10 q10)) ) ) )
  and stations y12 y13 = fresh (q1 q2) (y12 === o () &&& (y13 === Std.List.nil ()) ||| (y12 === s q1 &&& (y13 === Std.( % ) (o ()) q2) &&& stations q1 q2))
  and eqNat y14 y15 = fresh (q1 q2) (y15 === o () &&& (y14 === o ()) ||| (y15 === s q1 &&& (y14 === s q2) &&& eqNat q2 q1))
  and checkStep y16 y17 y18 y19 = fresh (q1 q2 q3 q4) (y18 === st q1 q2 q3 &&& (y19 === s q4 &&& (goe q1 q4 &&& goe q2 q4)))
  and stepCalcFuel y20 y21 y22 y23 y25 y26 = fresh q1 (step y20 y21 y22 q1 y26 &&& _calcFuel y20 y21 y23 q1 y25)
  and goe y27 y28 =
    fresh (q1 q2 q3)
      (y27 === s q1 &&& (q1 === o () &&& (y28 === o ()) ||| (q1 === s q2 &&& (y28 === o ())) ||| (q1 === s q2 &&& (y28 === s q3) &&& _goe q2 q3)))
  and _goe y29 y30 = fresh (q1 q2) (y29 === o () &&& (y30 === o ()) ||| (y29 === s q1 &&& (y30 === o ())) ||| (y29 === s q1 &&& (y30 === s q2) &&& _goe q1 q2))
  and step y31 y32 y33 y34 y35 = fresh (q1 q2 q3 q4 q5) (y33 === st q1 q2 q3 &&& (y34 === st q4 q5 q3) &&& (sub q1 y35 q4 &&& sub q2 y35 q5))
  and _calcFuel y36 y37 y38 y39 y40 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y38 === Std.List.nil () &&& (y37 === y40) &&& isFinishState y36 y39
      ||| ( y38 === Std.( % ) q1 q2
          &&& ( q1 === fill ()
              &&& (__checkStep y36 y37 y39 &&& __step y36 y37 y39 q3 &&& calcFuel q2 y36 y37 q4 q3 &&& __getFuel y37 y39 q5 &&& add q4 y40 q5)
              ||| ( q1 === pour q6
                  &&& (___checkStep y36 y37 y39 q6 &&& ___step y36 y37 y39 q3 q6 &&& calcFuel q2 y36 y37 q4 q3 &&& ___getFuel y37 y39 q5 q6 &&& add q4 y40 q5)
                  ) ) ) )
  and sub y41 y42 y43 =
    fresh (q1 q2)
      (y42 === o () &&& (y41 === y43) ||| (y42 === s q1 &&& (y41 === o ()) &&& (y43 === o ())) ||| (y42 === s q1 &&& (y41 === s q2) &&& sub q2 q1 y43))
  and isFinishState y44 y45 =
    fresh (q1 q2 q3 q4 q5) (y45 === st q1 q2 q3 &&& (q1 === o () &&& (y44 === o ()) ||| (q1 === s q4 &&& (y44 === s q5) &&& eqNat q5 q4)))
  and add y50 y51 y52 = fresh (q1 q2) (y52 === o () &&& (y50 === y51) ||| (y52 === s q1 &&& (y51 === s q2) &&& add y50 q2 q1))
  and _checkStep y53 y54 y55 y56 = fresh (q1 q2 q3 q4) (y55 === st q1 q2 q3 &&& (y56 === s q4 &&& (addGoe y53 q1 q4 &&& goe q2 q4)))
  and _step y57 y58 y59 y60 y61 = fresh (q1 q2 q3 q4 q5) (y59 === st q1 q2 q3 &&& (y60 === st q4 q5 q3) &&& (add y61 q4 q1 &&& sub q2 y61 q5))
  and _getFuel y62 y63 y64 y65 = y64 === o ()
  and addGoe y66 y67 y69 = fresh q1 (_add y67 q1 y69 &&& _goe y66 q1)
  and _add y70 y71 y72 = fresh (q1 q2) (y70 === o () &&& (y71 === s y72) ||| (y70 === s q1 &&& (y71 === s q2) &&& _add q1 q2 y72))
  and __checkStep y73 y74 y75 = fresh (q1 q2 q3) (y75 === st (o ()) q1 q2 &&& _eqNat y74 q1 ||| (y75 === st (s q3) q1 q2 &&& (_eqNat y74 q1 &&& elem q2 q3)))
  and __step y76 y77 y78 y79 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y78
      === st (o ()) q1 q2
      &&& (y79 === st (o ()) y77 q2)
      ||| (y78 === st (s q3) q1 q2 &&& (y79 === st (s q3) y77 q4) &&& (elemAdd q1 q2 q3 q5 &&& _goe q5 y77 &&& subSetForElem y77 q2 q3 q5 q4))
      ||| (y78 === st (s q3) q1 q2 &&& (y79 === st (s q3) q5 q6) &&& (elemAdd q1 q2 q3 q5 &&& __goe y77 q5 &&& _setForElem q2 q3 q6)) )
  and __getFuel y80 y81 y82 = fresh (q1 q2 q3) (y81 === st (o ()) q1 q2 &&& sub y80 q1 y82 ||| (y81 === st (s q3) q1 q2 &&& (y82 === o ())))
  and _eqNat y83 y84 =
    fresh (q1 q2 q3) (y84 === o () &&& (y83 === s q1) ||| (y84 === s q2 &&& (y83 === o ())) ||| (y84 === s q2 &&& (y83 === s q3) &&& _eqNat q3 q2))
  and elem y85 y86 = fresh (q1 q2 q3 q4) (y85 === Std.( % ) (s q1) q2 &&& (y86 === o ()) ||| (y85 === Std.( % ) q3 q2 &&& (y86 === s q4) &&& elem q2 q4))
  and elemAdd y88 y89 y90 y92 = fresh q1 (_elem y89 y90 q1 &&& add q1 y92 y88)
  and subSetForElem y93 y94 y95 y96 y97 = fresh q1 (sub y96 y93 q1 &&& setForElem y94 y95 y97 q1)
  and _elem y99 y100 y101 =
    fresh (q1 q2 q3) (y99 === Std.( % ) y101 q1 &&& (y100 === o ()) ||| (y99 === Std.( % ) q2 q1 &&& (y100 === s q3) &&& _elem q1 q3 y101))
  and setForElem y102 y103 y104 y105 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9)
      ( y102 === Std.( % ) q1 q2
      &&& (y103 === o ())
      &&& (y104 === Std.( % ) y105 q2)
      ||| ( y102 === Std.( % ) q1 q2
          &&& (y103 === s q3)
          &&& (y104 === Std.( % ) q1 q4)
          &&& ( q2 === Std.( % ) q5 q6
              &&& (q3 === o ())
              &&& (q4 === Std.( % ) q7 q6)
              &&& add q5 q7 y105
              ||| (q2 === Std.( % ) q5 q6 &&& (q3 === s q8) &&& (q4 === Std.( % ) q5 q9) &&& addForElem y105 q6 q8 q9) ) ) )
  and addForElem y106 y107 y108 y109 =
    fresh (q1 q2 q3 q4 q5)
      ( y107 === Std.( % ) q1 q2
      &&& (y108 === o ())
      &&& (y109 === Std.( % ) q3 q2)
      &&& add q1 q3 y106
      ||| (y107 === Std.( % ) q1 q2 &&& (y108 === s q4) &&& (y109 === Std.( % ) q1 q5) &&& addForElem y106 q2 q4 q5) )
  and __goe y110 y111 = fresh (q1 q2 q3) (y111 === o () &&& (y110 === s q1) ||| (y111 === s q2 &&& (y110 === s q3) &&& __goe q3 q2))
  and _setForElem y112 y113 y114 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8)
      ( y112 === Std.( % ) q1 q2
      &&& (y113 === o ())
      &&& (y114 === Std.( % ) (o ()) q2)
      ||| ( y112 === Std.( % ) q1 q2
          &&& (y113 === s q3)
          &&& (y114 === Std.( % ) q1 q4)
          &&& ( q2 === Std.( % ) q5 q6
              &&& (q3 === o ())
              &&& (q4 === Std.( % ) q5 q6)
              ||| (q2 === Std.( % ) q5 q6 &&& (q3 === s q7) &&& (q4 === Std.( % ) q5 q8) &&& _addForElem q6 q7 q8) ) ) )
  and _addForElem y115 y116 y117 =
    fresh (q1 q2 q3 q4)
      ( y115 === Std.( % ) q1 q2
      &&& (y116 === o ())
      &&& (y117 === Std.( % ) q1 q2)
      ||| (y115 === Std.( % ) q1 q2 &&& (y116 === s q3) &&& (y117 === Std.( % ) q1 q4) &&& _addForElem q2 q3 q4) )
  and ___checkStep y118 y119 y120 y121 = fresh (q1 q2 q3 q4) (y120 === st (s q1) q2 q3 &&& (y121 === s q4 &&& (__eqNat y118 q1 &&& goe q2 q4)))
  and ___step y122 y123 y124 y125 y126 =
    fresh (q1 q2 q3 q4 q5) (y124 === st (s q1) q2 q3 &&& (y125 === st (s q1) q4 q5) &&& (sub q2 y126 q4 &&& addForElem y126 q3 q1 q5))
  and ___getFuel y127 y128 y129 y130 = y129 === o ()
  and __eqNat y131 y132 = fresh q1 (y131 === o () ||| (y131 === s q1 &&& _eqNat q1 y132))
  and checkStepStepCalcFuel y133 y134 y135 y136 y138 y139 =
    fresh q1 (_checkStep y133 y134 y135 y139 &&& _step y133 y134 y135 q1 y139 &&& _calcFuel y133 y134 y136 q1 y138)
  in
  checkAnswer x0 x1 x2 x3
