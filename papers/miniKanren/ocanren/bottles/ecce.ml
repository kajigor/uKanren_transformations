open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 =
  let rec checkAnswer z1 z2 z3 z4 = fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === !!true) &&& checkAnswer__1 fA fB fC)
  and checkAnswer__1 z1 z2 z3 =
    fresh (fA) (z1 === Std.List.nil () &&& (z2 === fA) &&& (z3 === o ()))
    ||| fresh (fE fD fC fB fA) (z1 === Std.( % ) (Pair.pair fA fB) fC &&& (z2 === fD) &&& (z3 === fE) &&& checkStep_conj__2 fA fB fD fC fE)
  and checkStep_conj__2 z1 z2 z3 z4 z5 =
    fresh (fD fC fB fA)
      (z1 === fill () &&& (z2 === fst_ ()) &&& (z3 === Pair.pair fA fB) &&& (z4 === fC) &&& (z5 === fD) &&& checkAnswer_0__4 fA (o ()) fC fA fB fD)
    ||| fresh (fD fC fB fA)
          (z1 === fill () &&& (z2 === snd_ ()) &&& (z3 === Pair.pair fA fB) &&& (z4 === fC) &&& (z5 === fD) &&& checkAnswer_0__4 (o ()) fB fC fA fB fD)
    ||| fresh (fC fB fA)
          (z1 === empty () &&& (z2 === fst_ ()) &&& (z3 === Pair.pair (o ()) fA) &&& (z4 === fB) &&& (z5 === fC) &&& checkAnswer_0__3 fB (o ()) fA fC)
    ||| fresh (fC fB fA)
          (z1 === empty () &&& (z2 === snd_ ()) &&& (z3 === Pair.pair fA (o ())) &&& (z4 === fB) &&& (z5 === fC) &&& checkAnswer_0__3 fB fA (o ()) fC)
  and checkAnswer_0__3 z1 z2 z3 z4 =
    fresh (fB fA) (z1 === Std.List.nil () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === o ()))
    ||| fresh (fF fE fD fC fB fA)
          (z1 === Std.( % ) (Pair.pair fA fB) fC &&& (z2 === fD) &&& (z3 === fE) &&& (z4 === fF) &&& checkStep_conj__2 fA fB (Pair.pair fD fE) fC fF)
  and checkAnswer_0__4 z1 z2 z3 z4 z5 z6 =
    fresh (fE fD fC fB fA)
      (z1 === fA &&& (z2 === fB) &&& (z3 === Std.List.nil ()) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& isFinishState__5 fA fB fE)
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB)
          &&& (z3 === Std.( % ) (Pair.pair fC fD) fE)
          &&& (z4 === fF) &&& (z5 === fG) &&& (z6 === fH) &&& checkStep_conj__6 fA fB fC fD fF fG fE fH )
  and isFinishState__5 z1 z2 z3 =
    fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& fancyEq__16 fA fC &&& fancyEq__18 fB fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& fancyEq_conj__17 fA fC fB)
  and checkStep_conj__6 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA)
      &&& (z3 === fill ())
      &&& (z4 === fst_ ())
      &&& (z5 === fB) &&& (z6 === fC) &&& (z7 === fD) &&& (z8 === fE) &&& checkAnswer_0__4 fB fA fD fB fC fE )
    ||| fresh (fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === o ())
          &&& (z3 === fill ())
          &&& (z4 === snd_ ())
          &&& (z5 === fB) &&& (z6 === fC) &&& (z7 === fD) &&& (z8 === fE) &&& checkAnswer_0__4 fA fC fD fB fC fE )
    ||| fresh (fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB)
          &&& (z3 === empty ())
          &&& (z4 === fst_ ())
          &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& fancyEq__16 fA fC
          &&& checkAnswer_0__4 (o ()) fB fE fC fD fF )
    ||| fresh (fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB)
          &&& (z3 === empty ())
          &&& (z4 === snd_ ())
          &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& fancyEq__16 fB fD
          &&& checkAnswer_0__4 fA (o ()) fE fC fD fF )
    ||| fresh (fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB)
          &&& (z3 === pour ())
          &&& (z4 === fst_ ())
          &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& fancyEq__7 fB fD &&& doStep_conj__15 fA fB fC fD fE fF )
    ||| fresh (fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === s fB)
          &&& (z3 === pour ())
          &&& (z4 === snd_ ())
          &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& fancyEq__7 fA fC &&& doStep_conj__8 fA fB fC fD fE fF )
  and fancyEq__7 z1 z2 =
    fresh (fA) (z1 === o () &&& (z2 === s fA)) ||| fresh (fA) (z1 === s fA &&& (z2 === o ())) ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& fancyEq__7 fA fB)
  and doStep_conj__8 z1 z2 z3 z4 z5 z6 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF)
      &&& add__12 fA (s fB) (s fG)
      &&& greater__11 fG fC
      &&& add__12 fA (s fB) (s fH)
      &&& sub_conj__13 fH fC fI fC fI fE fC fD fF )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF)
          &&& add_conj__9 fA (s fB) (s fG) fG fC (s fH)
          &&& checkAnswer_0__4 (s fH) (o ()) fE (s fC) fD fF )
  and add_conj__9 z1 z2 z3 z4 z5 z6 =
    fresh (fC fB fA) (z1 === o () &&& (z2 === fA) &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fA) &&& greater__10 fB fC)
    ||| fresh (fF fE fD fC fB fA)
          (z1 === s fA &&& (z2 === fB) &&& (z3 === s fC) &&& (z4 === s fD) &&& (z5 === s fE) &&& (z6 === s fF) &&& add_conj__9 fA fB fC fD fE fF)
  and greater__10 z1 z2 = fresh (fA) (z1 === o () &&& (z2 === fA)) ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& greater__10 fA fB)
  and greater__11 z1 z2 = fresh (fA) (z1 === fA &&& (z2 === o ())) ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& greater__11 fA fB)
  and add__12 z1 z2 z3 =
    fresh (fA) (z1 === o () &&& (z2 === fA) &&& (z3 === fA)) ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === fB) &&& (z3 === s fC) &&& add__12 fA fB fC)
  and sub_conj__13 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === fA
      &&& (z2 === o ())
      &&& (z3 === s fA)
      &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG) &&& checkAnswer_0__4 fB fC fD fE fF fG )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === s fB)
          &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI)
          &&& sub_conj__14 fA fB fC fD fE fF fG fH fI )
  and sub_conj__14 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === fA
      &&& (z2 === o ())
      &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG) &&& checkAnswer_0__4 fB fC fD fE fF fG )
    ||| fresh (fG fF fE fD fC fB fA)
          ( z1 === o ()
          &&& (z2 === s fA)
          &&& (z3 === o ())
          &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG) &&& checkAnswer_0__4 fB fC fD fE fF fG )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === s fB)
          &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI)
          &&& sub_conj__14 fA fB fC fD fE fF fG fH fI )
  and doStep_conj__15 z1 z2 z3 z4 z5 z6 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& add__12 fA fB fG &&& greater__11 fG fD
      &&& add__12 fA fB fH &&& sub_conj__13 fH fD fI fI fD fE fC fD fF )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB) &&& (z3 === fC)
          &&& (z4 === s fD)
          &&& (z5 === fE) &&& (z6 === fF) &&& add_conj__9 fA fB fG fG fD fH
          &&& checkAnswer_0__4 (o ()) (s fH) fE fC (s fD) fF )
  and fancyEq__16 z1 z2 = z1 === o () &&& (z2 === o ()) ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& fancyEq__16 fA fB)
  and fancyEq_conj__17 z1 z2 z3 =
    fresh (fB fA) (z1 === o () &&& (z2 === s fA) &&& (z3 === s fB) &&& fancyEq__16 fB fA)
    ||| fresh (fA) (z1 === s fA &&& (z2 === o ()) &&& (z3 === o ()))
    ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === s fB) &&& (z3 === s fC) &&& fancyEq_conj__17 fA fB fC)
  and fancyEq__18 z1 z2 =
    z1 === o ()
    &&& (z2 === o ())
    ||| fresh (fA) (z1 === o () &&& (z2 === s fA))
    ||| fresh (fA) (z1 === s fA &&& (z2 === o ()))
    ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& fancyEq__18 fA fB)
  in
  checkAnswer__1 y1 y2 y3
