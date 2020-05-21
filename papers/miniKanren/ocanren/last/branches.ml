open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 =
  let rec evalo y0 y1 =
    fresh (q1 q2 q3 q4)
      ( y1 === conj q1 q2 &&& _evaloEvaloAndo y0 q2 q3 q1 q3
      ||| (y1 === disj q1 q2 &&& _evaloEvaloOro y0 q2 q3 q1 q3)
      ||| (y1 === neg q1 &&& evaloNoto y0 q1)
      ||| (y1 === var q4 &&& elemo !!true q4 y0) )
  and _evaloEvaloAndo y7 y8 y9 y11 y12 =
    fresh (q1 q2 q3 q4)
      ( y11 === conj q1 q2
      &&& (_evaloEvaloAndo y7 y8 y9 q1 q3 &&& _evaloAndo y12 y7 q2 q3)
      ||| (y11 === disj q1 q2 &&& (_evaloEvaloAndo y7 y8 y9 q1 q3 &&& evaloOro y12 y7 q2 q3))
      ||| (y11 === neg q1 &&& (_evaloEvaloAndo y7 y8 y9 q1 q3 &&& noto y12 q3))
      ||| (y11 === var q4 &&& elemoEvaloAndo y7 y8 y9 y12 q4) )
  and elemoEvaloAndo y13 y14 y15 y17 y18 =
    fresh (q1 q2 q3)
      ( y18 === Nat.zero
      &&& (y13 === y17 % q1)
      &&& _evaloAndo !!true (y17 % q1) y14 y15
      ||| (y18 === Nat.succ q2 &&& (y13 === q3 % q1) &&& (_evaloAndo !!true (q3 % q1) y14 y15 &&& elemo y17 q2 q1)) )
  and elemo y19 y20 y21 = fresh (q1 q2 q3) (y20 === Nat.zero &&& (y21 === y19 % q1) ||| (y20 === Nat.succ q2 &&& (y21 === q3 % q1) &&& elemo y19 q2 q1))
  and _evaloAndo y26 y27 y28 y29 =
    fresh (q1 q2 q3 q4 q5)
      ( y28 === conj q1 q2
      &&& (_evaloAndo q3 y27 q2 q4 &&& _evalo y27 q1 q4 &&& ando y26 y29 q3)
      ||| (y28 === disj q1 q2 &&& (evaloOro q3 y27 q2 q4 &&& _evalo y27 q1 q4 &&& ando y26 y29 q3))
      ||| (y28 === neg q1 &&& (_evalo y27 q1 q4 &&& noto q3 q4 &&& ando y26 y29 q3))
      ||| (y28 === var q5 &&& elemoAndo y26 y27 y29 q5) )
  and elemoAndo y31 y32 y33 y35 =
    fresh (q1 q2 q3 q4) (y35 === Nat.zero &&& (y32 === q1 % q2) &&& ando y31 y33 q1 ||| (y35 === Nat.succ q3 &&& (y32 === q4 % q2) &&& elemoAndo y31 q2 y33 q3))
  and evaloOro y36 y37 y38 y39 =
    fresh (q1 q2 q3 q4 q5)
      ( y38 === conj q1 q2
      &&& (_evaloAndo q3 y37 q2 q4 &&& _evalo y37 q1 q4 &&& oro y36 y39 q3)
      ||| (y38 === disj q1 q2 &&& (evaloOro q3 y37 q2 q4 &&& _evalo y37 q1 q4 &&& oro y36 y39 q3))
      ||| (y38 === neg q1 &&& (_evalo y37 q1 q4 &&& noto q3 q4 &&& oro y36 y39 q3))
      ||| (y38 === var q5 &&& elemoOro y36 y37 y39 q5) )
  and elemoOro y41 y42 y43 y45 =
    fresh (q1 q2 q3 q4) (y45 === Nat.zero &&& (y42 === q1 % q2) &&& oro y41 y43 q1 ||| (y45 === Nat.succ q3 &&& (y42 === q4 % q2) &&& elemoOro y41 q2 y43 q3))
  and ando y46 y47 y48 =
    y47 === !!false &&& (y48 === !!false) &&& nando y46
    ||| (y47 === !!false &&& (y48 === !!true) &&& nando y46)
    ||| (y47 === !!true &&& (y48 === !!false) &&& nando y46)
    ||| (y47 === !!true &&& (y48 === !!true) &&& _nando y46)
  and nando y49 = y49 === !!false
  and _nando y50 = y50 === !!true
  and _evalo y51 y52 y53 =
    fresh (q1 q2 q3 q4)
      ( y52 === conj q1 q2
      &&& (_evaloAndo y53 y51 q2 q3 &&& _evalo y51 q1 q3)
      ||| (y52 === disj q1 q2 &&& (evaloOro y53 y51 q2 q3 &&& _evalo y51 q1 q3))
      ||| (y52 === neg q1 &&& (_evalo y51 q1 q3 &&& noto y53 q3))
      ||| (y52 === var q4 &&& elemo y53 q4 y51) )
  and oro y54 y55 y56 = y55 === !!false &&& nandoNando y54 y56 ||| (y55 === !!true &&& _nandoNando y54 y56)
  and nandoNando y57 y58 = y58 === !!false &&& nando y57 ||| (y58 === !!true &&& __nando y57)
  and __nando y60 = y60 === !!true
  and _nandoNando y61 y62 = y62 === !!false &&& ___nando y61 ||| (y62 === !!true &&& _nando y61)
  and ___nando y64 = y64 === !!true
  and noto y65 y66 = y66 === !!false &&& (y65 === !!true) ||| (y66 === !!true &&& (y65 === !!false))
  and _evaloEvaloOro y72 y73 y74 y76 y77 =
    fresh (q1 q2 q3 q4)
      ( y76 === conj q1 q2
      &&& (_evaloEvaloOro y72 y73 y74 q1 q3 &&& _evaloAndo y77 y72 q2 q3)
      ||| (y76 === disj q1 q2 &&& (_evaloEvaloOro y72 y73 y74 q1 q3 &&& evaloOro y77 y72 q2 q3))
      ||| (y76 === neg q1 &&& (_evaloEvaloOro y72 y73 y74 q1 q3 &&& noto y77 q3))
      ||| (y76 === var q4 &&& elemoEvaloOro y72 y73 y74 y77 q4) )
  and elemoEvaloOro y78 y79 y80 y82 y83 =
    fresh (q1 q2 q3)
      ( y83 === Nat.zero
      &&& (y78 === y82 % q1)
      &&& evaloOro !!true (y82 % q1) y79 y80
      ||| (y83 === Nat.succ q2 &&& (y78 === q3 % q1) &&& (evaloOro !!true (q3 % q1) y79 y80 &&& elemo y82 q2 q1)) )
  and evaloNoto y84 y85 =
    fresh (q1 q2 q3 q4 q5)
      ( y85 === conj q1 q2
      &&& (_evaloAndo q3 y84 q2 q4 &&& _evalo y84 q1 q4 &&& noto !!true q3)
      ||| (y85 === disj q1 q2 &&& (evaloOro q3 y84 q2 q4 &&& _evalo y84 q1 q4 &&& noto !!true q3))
      ||| (y85 === neg q1 &&& (_evalo y84 q1 q4 &&& noto q3 q4 &&& noto !!true q3))
      ||| (y85 === var q5 &&& elemoNoto y84 q5) )
  and elemoNoto y87 y89 =
    fresh (q1 q2 q3 q4) (y89 === Nat.zero &&& (y87 === q1 % q2) &&& noto !!true q1 ||| (y89 === Nat.succ q3 &&& (y87 === q4 % q2) &&& elemoNoto q2 q3))
  in
  evalo x0 x1