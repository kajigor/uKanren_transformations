open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 y4 =
  let rec checkAnswer z1 z2 z3 z4 = fresh (fD fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === Option.some fD) &&& checkAnswer__1 fA fB fC fD)
  and checkAnswer__1 z1 z2 z3 z4 =
    fresh fA (z1 === Std.List.nil () &&& (z2 === o ()) &&& (z3 === fA) &&& (z4 === fA))
    ||| fresh (fG fF fE fD fC fB fA)
          ( z1
          === Std.( % ) (right (s fA)) fB
          &&& (z2 === s fC)
          &&& (z3 === s fD)
          &&& (z4 === fE) &&& stations__3 fC fF &&& goe_conj__5 fC fA fD fD fG
          &&& calcFuel__6 (s fA) fG (o ()) fF fB fC fD fE )
  and stations__3 z1 z2 = z1 === o () &&& (z2 === Std.List.nil ()) ||| fresh (fB fA) (z1 === s fA &&& (z2 === Std.( % ) (o ()) fB) &&& stations__3 fA fB)
  and goe_conj__5 z1 z2 z3 z4 z5 =
    fresh (fB fA) (z1 === o () &&& (z2 === o ()) &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fB) &&& goe__19 fA)
    ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === o ()) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fC) &&& goe__19 fB)
    ||| fresh (fE fD fC fB fA) (z1 === s fA &&& (z2 === s fB) &&& (z3 === s fC) &&& (z4 === fD) &&& (z5 === fE) &&& goe_conj__18 fA fB fC &&& sub__65 fD fB fE)
  and calcFuel__6 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fF fE fD fC fB fA)
      ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
      &&& (z5 === Std.List.nil ())
      &&& (z6 === fE) &&& (z7 === fF)
      &&& (z8 === s fF)
      &&& eqNat__12 fA fE )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
          &&& (z5 === Std.( % ) fE fF)
          &&& (z6 === fG) &&& (z7 === fH) &&& (z8 === fI) &&& isMove_conj__7 fE fA fB fC fD fG fH fF fI )
  and isMove_conj__7 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === fill () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH)
      &&& checkStep_conj__8 fA fB fC fD fE fF fG fH )
    ||| fresh (fL fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1
          === pour (s fA)
          &&& (z2 === s fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& eqNat__24 fB fF &&& goe_conj__9 fC fA fJ
          &&& addForElem__10 fD fE fB fA fK fL &&& calcFuel__11 fB fJ fK fL fH fF fG fI )
  and checkStep_conj__8 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG)
      &&& eqNat_conj__27 fA fE fB fC fF fD fG )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& eqNat__28 fB fF
          &&& elem__29 fC fD fA &&& step_conj__30 fA fB fC fD fE fF fG fH )
  and goe_conj__9 z1 z2 z3 =
    z1 === o ()
    &&& (z2 === o ())
    &&& (z3 === o ())
    ||| fresh fA (z1 === s fA &&& (z2 === o ()) &&& (z3 === s fA))
    ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === s fB) &&& (z3 === fC) &&& goe_conj__9 fA fB fC)
  and addForElem__10 z1 z2 z3 z4 z5 z6 =
    fresh (fD fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === o ()) &&& (z4 === fC) &&& (z5 === s fD) &&& (z6 === fB) &&& add__26 fC fA fD)
    ||| fresh (fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === Std.( % ) fB fC)
          &&& (z3 === s fD)
          &&& (z4 === fE) &&& (z5 === fA)
          &&& (z6 === Std.( % ) fF fG)
          &&& addForElem__10 fB fC fD fE fF fG )
  and calcFuel__11 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
      &&& (z5 === Std.List.nil ())
      &&& (z6 === fE) &&& (z7 === fF)
      &&& (z8 === s fF)
      &&& eqNat__12 fA fE )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
          &&& (z5 === Std.( % ) fE fF)
          &&& (z6 === fG) &&& (z7 === fH) &&& (z8 === fI) &&& isMove_conj__13 fE fA fB fC fD fG fH fF fI )
  and eqNat__12 z1 z2 = z1 === o () &&& (z2 === o ()) ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& eqNat__12 fA fB)
  and isMove_conj__13 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1
      === left (s fA)
      &&& (z2 === fB)
      &&& (z3 === s fC)
      &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& goe_conj__14 fB fA fC fD fE fH fF fG fI )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1
          === right (s fA)
          &&& (z2 === fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE)
          &&& (z6 === s fF)
          &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& add_conj__15 fB fA fF fC &&& add_conj__16 fB fA fJ fC fJ fD fE fH fF fG fI )
  and goe_conj__14 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === o ()
      &&& (z2 === o ())
      &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG) &&& goe_conj__20 fA fB fC fD fE fF fG )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === o ())
          &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH)
          &&& goe_conj__21 fB fA fC fD fE fF fG fH )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === s fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& goe_conj__14 fA fB fC fD fE fF fG fH fI )
  and add_conj__15 z1 z2 z3 z4 =
    fresh (fC fB fA) (z1 === o () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& goe_conj__18 fB fA fC)
    ||| fresh (fD fC fB fA) (z1 === s fA &&& (z2 === fB) &&& (z3 === s fC) &&& (z4 === fD) &&& add_conj__15 fA fB fC fD)
  and add_conj__16 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG)
      &&& (z10 === fH) &&& (z11 === fI) &&& sub_conj__17 fB fA fC fD fE fF fG fH fI )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& (z10 === fJ) &&& (z11 === fK)
          &&& add_conj__16 fA fB fC fD fE fF fG fH fI fJ fK )
  and sub_conj__17 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === fA
      &&& (z2 === o ())
      &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH)
      &&& calcFuel__6 (s (s fB)) fA fC fD fE (s fF) fG fH )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === o ()
          &&& (z2 === s fA)
          &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH)
          &&& calcFuel__6 (s (s fB)) (o ()) fC fD fE (s fF) fG fH )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === s fB)
          &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI)
          &&& sub_conj__17 fA fB fC fD fE fF fG fH fI )
  and goe_conj__18 z1 z2 z3 =
    fresh fA (z1 === o () &&& (z2 === o ()) &&& (z3 === fA) &&& goe__19 fA)
    ||| fresh (fB fA) (z1 === s fA &&& (z2 === o ()) &&& (z3 === fB) &&& goe__19 fB)
    ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === s fB) &&& (z3 === s fC) &&& goe_conj__18 fA fB fC)
  and goe__19 z1 = z1 === o () ||| fresh fA (z1 === s fA)
  and goe_conj__20 z1 z2 z3 z4 z5 z6 z7 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fB)
      &&& (z4 === Std.( % ) (fill ()) (Std.( % ) (right (s fC)) fD))
      &&& (z5 === fE) &&& (z6 === fF)
      &&& (z7 === s fG)
      &&& goe_conj__5 fE fC fF fF fH
      &&& calcFuel__6 (s fC) fH fA fB fD fE fF fI
      &&& add__26 fF fI fG )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC)
          &&& (z4 === Std.( % ) (fill ()) (Std.( % ) (right (s fD)) fE))
          &&& (z5 === fF) &&& (z6 === fG) &&& (z7 === fH) &&& eqNat__24 fA fG &&& goe_conj__5 fF fD fG fG fI
          &&& calcFuel__6 (s fD) fI fB fC fE fF fG fJ
          &&& sub_conj__25 fG fA fJ fH )
  and goe_conj__21 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG)
      &&& calcFuel__6 (s fA) (o ()) fB fC fD fE fF fG )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH)
          &&& calcFuel__6 (s fB) (s fA) fC fD fE fF fG fH )
  and eqNat__24 z1 z2 =
    fresh fA (z1 === o () &&& (z2 === s fA)) ||| fresh fA (z1 === s fA &&& (z2 === o ())) ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& eqNat__24 fA fB)
  and sub_conj__25 z1 z2 z3 z4 =
    fresh (fC fB fA) (z1 === fA &&& (z2 === o ()) &&& (z3 === fB) &&& (z4 === fC) &&& add__26 fA fB fC)
    ||| fresh (fB fA) (z1 === o () &&& (z2 === s fA) &&& (z3 === fB) &&& (z4 === fB))
    ||| fresh (fD fC fB fA) (z1 === s fA &&& (z2 === s fB) &&& (z3 === fC) &&& (z4 === fD) &&& sub_conj__25 fA fB fC fD)
  and add__26 z1 z2 z3 =
    fresh fA (z1 === o () &&& (z2 === fA) &&& (z3 === fA)) ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === fB) &&& (z3 === s fC) &&& add__26 fA fB fC)
  and eqNat_conj__27 z1 z2 z3 z4 z5 z6 z7 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC)
      &&& (z5 === Std.( % ) (right (s fD)) fE)
      &&& (z6 === fF)
      &&& (z7 === s fG)
      &&& goe_conj__5 fF fD fA fA fH
      &&& calcFuel__6 (s fD) fH fB fC fE fF fA fI
      &&& add__26 fA fI fG )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
          &&& (z5 === Std.( % ) (right (s fE)) fF)
          &&& (z6 === fG) &&& (z7 === fH) &&& eqNat__24 fA fB &&& goe_conj__5 fG fE fB fB fI
          &&& calcFuel__6 (s fE) fI fC fD fF fG fB fJ
          &&& sub_conj__25 fB fA fJ fH )
  and eqNat__28 z1 z2 = fresh fA (z1 === o () &&& (z2 === fA)) ||| fresh (fB fA) (z1 === s fA &&& (z2 === fB) &&& eqNat__24 fA fB)
  and elem__29 z1 z2 z3 =
    fresh (fB fA) (z1 === s fA &&& (z2 === fB) &&& (z3 === o ()))
    ||| fresh (fD fC fB fA) (z1 === fA &&& (z2 === Std.( % ) fB fC) &&& (z3 === s fD) &&& elem__29 fB fC fD)
  and step_conj__30 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH)
      &&& elem_conj__31 fC fD fA fB fF fG fE fH )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH)
          &&& elem_conj__32 fC fD fA fB fF fG fE fH )
  and elem_conj__31 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB)
      &&& (z3 === o ())
      &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& add_conj__49 fC fA fD fB fE fF fG )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === Std.( % ) fB fC)
          &&& (z3 === s fD)
          &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fG) &&& (z7 === fH) &&& (z8 === fI)
          &&& elem_conj__50 fB fC fD fE fF fJ fK fD fA fJ fK fG fH fI )
  and elem_conj__32 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB)
      &&& (z3 === o ())
      &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& add_conj__34 fC fA fH fD &&& calcFuel__43 fH fB fE fF fD fG )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === Std.( % ) fB fC)
          &&& (z3 === s fD)
          &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fG) &&& (z7 === fH) &&& (z8 === fI)
          &&& elem_conj__33 fB fC fD fE fF fJ fK fD fA fJ fK fG fH fI )
  and elem_conj__33 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 =
    fresh (fL fK fJ fI fH fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB)
      &&& (z3 === o ())
      &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fA) &&& (z7 === fB) &&& (z8 === fE) &&& (z9 === fF) &&& (z10 === fG) &&& (z11 === fH) &&& (z12 === fI)
      &&& (z13 === fJ) &&& (z14 === fK) &&& add_conj__34 fC fA fL fD &&& calcFuel__35 fE fL fF fG fH fI fJ fD fK )
    ||| fresh (fO fN fM fL fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === Std.( % ) fB fC)
          &&& (z3 === s fD)
          &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fA)
          &&& (z7 === Std.( % ) fG fH)
          &&& (z8 === fI) &&& (z9 === fJ) &&& (z10 === fK) &&& (z11 === fL) &&& (z12 === fM) &&& (z13 === fN) &&& (z14 === fO)
          &&& elem_conj__33 fB fC fD fE fF fG fH fI fJ fK fL fM fN fO )
  and add_conj__34 z1 z2 z3 z4 =
    fresh (fB fA) (z1 === o () &&& (z2 === fA) &&& (z3 === fA) &&& (z4 === fB) &&& goe__42 fA fB)
    ||| fresh (fD fC fB fA) (z1 === s fA &&& (z2 === fB) &&& (z3 === s fC) &&& (z4 === s fD) &&& add_conj__34 fA fB fC fD)
  and calcFuel__35 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE)
      &&& (z6 === Std.List.nil ())
      &&& (z7 === s fF)
      &&& (z8 === fG)
      &&& (z9 === s fG)
      &&& eqNat__12 fA fF )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE)
          &&& (z6 === Std.( % ) fF fG)
          &&& (z7 === fH) &&& (z8 === fI) &&& (z9 === fJ) &&& isMove_conj__36 fF fA fB fC fD fE fH fI fG fJ )
  and isMove_conj__36 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 =
    fresh (fJ fI fH fG fF fE fD fC fB fA)
      ( z1
      === left (s fA)
      &&& (z2 === fB)
      &&& (z3 === s fC)
      &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& (z10 === fJ)
      &&& goe_conj__37 fB fA fC fD fE fF fI fG fH fJ )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1
          === right (s fA)
          &&& (z2 === fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF)
          &&& (z7 === s (s fG))
          &&& (z8 === fH) &&& (z9 === fI) &&& (z10 === fJ) &&& add_conj__15 fB fA fG fC
          &&& add_conj__16 fB fA fK fC (s fK) fD (Std.( % ) fE fF) fI (s fG) fH fJ )
  and goe_conj__37 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1 === fA
      &&& (z2 === o ())
      &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH) &&& (z10 === fI)
      &&& goe_conj__21 fB fA fC (Std.( % ) fD fE) fF fG fH fI )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === s fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& (z10 === fJ)
          &&& goe_conj__38 fA fB fC fD fE fF fG fH fI fJ )
  and goe_conj__38 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === o ()
      &&& (z2 === o ())
      &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG) &&& (z10 === fH)
      &&& goe_conj__39 fA fB fC fD fE fF fG fH )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === o ())
          &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH) &&& (z10 === fI)
          &&& goe_conj__21 fB fA fC (Std.( % ) fD fE) fF fG fH fI )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === s fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& (z10 === fJ)
          &&& goe_conj__38 fA fB fC fD fE fF fG fH fI fJ )
  and goe_conj__39 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fJ fI fH fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC)
      &&& (z5 === Std.( % ) (fill ()) (Std.( % ) (right (s fD)) fE))
      &&& (z6 === fF) &&& (z7 === fG)
      &&& (z8 === s fH)
      &&& goe_conj__5 fF fD fG fG fI
      &&& calcFuel__6 (s fD) fI fA (Std.( % ) fB fC) fE fF fG fJ
      &&& add__26 fG fJ fH )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
          &&& (z5 === Std.( % ) (fill ()) (Std.( % ) (right (s fE)) fF))
          &&& (z6 === fG) &&& (z7 === fH) &&& (z8 === fI) &&& eqNat__24 fA fH &&& goe_conj__5 fG fE fH fH fJ
          &&& calcFuel__6 (s fE) fJ fB (Std.( % ) fC fD) fF fG fH fK
          &&& sub_conj__25 fH fA fK fI )
  and goe__42 z1 z2 = fresh fA (z1 === o () &&& (z2 === fA)) ||| fresh (fB fA) (z1 === s fA &&& (z2 === s fB) &&& goe__42 fA fB)
  and calcFuel__43 z1 z2 z3 z4 z5 z6 =
    fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === Std.List.nil ()) &&& (z4 === o ()) &&& (z5 === fC) &&& (z6 === s fC))
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fG) &&& isMove_conj__44 fC fA fB fE fF fD fG)
  and isMove_conj__44 z1 z2 z3 z4 z5 z6 z7 =
    fresh (fF fE fD fC fB fA)
      ( z1
      === left (s (o ()))
      &&& (z2 === s fA)
      &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& goe_conj__45 fA fB fE fC fD fF )
    ||| fresh (fG fF fE fD fC fB fA)
          ( z1
          === right (s fA)
          &&& (z2 === s fB)
          &&& (z3 === fC)
          &&& (z4 === s fD)
          &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG)
          &&& goe_conj__46 fD fA fB fA (o ()) fC fF fD fE fG )
  and goe_conj__45 z1 z2 z3 z4 z5 z6 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA)
      &&& (z3 === Std.( % ) (fill ()) (Std.( % ) (right (s fB)) fC))
      &&& (z4 === fD) &&& (z5 === fE)
      &&& (z6 === s fF)
      &&& goe_conj__5 fD fB fE fE fG
      &&& calcFuel__6 (s fB) fG (o ()) fA fC fD fE fH
      &&& add__26 fE fH fF )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB)
          &&& (z3 === Std.( % ) (fill ()) (Std.( % ) (right (s fC)) fD))
          &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fG) &&& eqNat__24 fA fF &&& goe_conj__5 fE fC fF fF fH
          &&& calcFuel__6 (s fC) fH (o ()) fB fD fE fF fI
          &&& sub_conj__25 fF fA fI fG )
  and goe_conj__46 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === o ()
      &&& (z2 === o ())
      &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG) &&& (z10 === fH)
      &&& goe_conj__21 fA (s fB) fC fD fE (s fF) fG fH )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === o ())
          &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH) &&& (z10 === fI)
          &&& goe_conj__21 fB (s fC) fD fE fF (s fG) fH fI )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === s fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& (z10 === fJ)
          &&& goe_conj__46 fA fB fC fD fE fF fG fH fI fJ )
  and add_conj__49 z1 z2 z3 z4 z5 z6 z7 =
    fresh (fF fE fD fC fB fA)
      (z1 === o () &&& (z2 === s fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& goe_conj__60 fA fB fB fC fD fE fF)
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& add__26 fA fB fH
          &&& goe_conj__60 fH fC fC fD fE fF fG )
  and elem_conj__50 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 =
    fresh (fM fL fK fJ fI fH fG fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB)
      &&& (z3 === o ())
      &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fB) &&& (z8 === fF) &&& (z9 === fG) &&& (z10 === fH) &&& (z11 === fI) &&& (z12 === fJ)
      &&& (z13 === fK) &&& (z14 === fL) &&& add_conj__51 fC fA fD fM &&& add__26 fM fA fE &&& calcFuel__52 fF fD fG fH fI fJ fK fL )
    ||| fresh (fO fN fM fL fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === Std.( % ) fB fC)
          &&& (z3 === s fD)
          &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fA)
          &&& (z7 === Std.( % ) fG fH)
          &&& (z8 === fI) &&& (z9 === fJ) &&& (z10 === fK) &&& (z11 === fL) &&& (z12 === fM) &&& (z13 === fN) &&& (z14 === fO)
          &&& elem_conj__50 fB fC fD fE fF fG fH fI fJ fK fL fM fN fO )
  and add_conj__51 z1 z2 z3 z4 =
    fresh (fC fB fA) (z1 === o () &&& (z2 === s fA) &&& (z3 === fB) &&& (z4 === fC) &&& goe_conj__9 fA fB fC)
    ||| fresh (fE fD fC fB fA) (z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& add__26 fA fB fE &&& goe_conj__9 fE fC fD)
  and calcFuel__52 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fF fE fD fC fB fA)
      ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE)
      &&& (z6 === Std.List.nil ())
      &&& (z7 === s fF)
      &&& (z8 === s fB)
      &&& eqNat__12 fA fF )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE)
          &&& (z6 === Std.( % ) fF fG)
          &&& (z7 === fH) &&& (z8 === fI) &&& isMove_conj__53 fF fA fB fC fD fE fH fG fI )
  and isMove_conj__53 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fI fH fG fF fE fD fC fB fA)
      ( z1
      === left (s fA)
      &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI)
      &&& goe_conj__54 fB fA fC fD fE fF fH fG fI )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1
          === right (s fA)
          &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF)
          &&& (z7 === s (s fG))
          &&& (z8 === fH) &&& (z9 === fI) &&& add_conj__15 fB fA fG fC
          &&& add_conj__16 fB fA fJ fC (s fJ) fD (Std.( % ) fE fF) fH (s fG) fC fI )
  and goe_conj__54 z1 z2 z3 z4 z5 z6 z7 z8 z9 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === fA
      &&& (z2 === o ())
      &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH)
      &&& goe_conj__21 fB fA fC (Std.( % ) fD fE) fF fG fB fH )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === s fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& goe_conj__55 fA fB fC fD fE fF fG fH fC fI )
  and goe_conj__55 z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === o ()
      &&& (z2 === o ())
      &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& (z8 === fF) &&& (z9 === fG) &&& (z10 === fH)
      &&& goe_conj__56 fA fB fC fD fE fF fG fH )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === o ())
          &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& (z8 === fG) &&& (z9 === fH) &&& (z10 === fI)
          &&& goe_conj__21 fB fA fC (Std.( % ) fD fE) fF fG (s fH) fI )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA
          &&& (z2 === s fB)
          &&& (z3 === s fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& (z9 === fI) &&& (z10 === fJ)
          &&& goe_conj__55 fA fB fC fD fE fF fG fH fI fJ )
  and goe_conj__56 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fJ fI fH fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC)
      &&& (z5 === Std.( % ) (fill ()) (Std.( % ) (right (s fD)) fE))
      &&& (z6 === fF) &&& (z7 === fG)
      &&& (z8 === s (s fH))
      &&& goe_conj__5 fF fD (s fG) (s fG) fI
      &&& calcFuel__6 (s fD) fI fA (Std.( % ) fB fC) fE fF (s fG) fJ
      &&& add__26 fG fJ fH )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
          &&& (z5 === Std.( % ) (fill ()) fE)
          &&& (z6 === fF) &&& (z7 === fG) &&& (z8 === fH) &&& eqNat_conj__59 fA fG fB fC fD fE fF fH )
  and eqNat_conj__59 z1 z2 z3 z4 z5 z6 z7 z8 =
    fresh (fJ fI fH fG fF fE fD fC fB fA)
      ( z1 === o () &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD)
      &&& (z6 === Std.( % ) (right (s fE)) fF)
      &&& (z7 === fG)
      &&& (z8 === s fH)
      &&& goe_conj__5 fG fE (s fA) (s fA) fI
      &&& calcFuel__6 (s fE) fI fB (Std.( % ) fC fD) fF fG (s fA) fJ
      &&& add__26 fA fJ fH )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE)
          &&& (z6 === Std.( % ) (right (s fF)) fG)
          &&& (z7 === fH) &&& (z8 === fI) &&& eqNat__24 fA fB
          &&& goe_conj__5 fH fF (s fB) (s fB) fJ
          &&& calcFuel__6 (s fF) fJ fC (Std.( % ) fD fE) fG fH (s fB) fK
          &&& sub_conj__25 fB fA fK fI )
  and goe_conj__60 z1 z2 z3 z4 z5 z6 z7 =
    fresh (fE fD fC fB fA)
      (z1 === o () &&& (z2 === o ()) &&& (z3 === fA) &&& (z4 === fB) &&& (z5 === fC) &&& (z6 === fD) &&& (z7 === fE) &&& calcFuel__61 fA fB fC fD fE)
    ||| fresh (fF fE fD fC fB fA)
          (z1 === s fA &&& (z2 === o ()) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& (z7 === fF) &&& calcFuel__62 fB fA fC fD fE fF)
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === s fA &&& (z2 === s fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& (z6 === fF) &&& (z7 === fG) &&& goe_conj__60 fA fB fC fD fE fF fG)
  and calcFuel__61 z1 z2 z3 z4 z5 =
    fresh (fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === Std.List.nil ()) &&& (z4 === o ()) &&& (z5 === s fA))
    ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& isMove_conj__64 fC fA fB fE fD fF)
  and calcFuel__62 z1 z2 z3 z4 z5 z6 =
    fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === Std.List.nil ()) &&& (z5 === o ()) &&& (z6 === s fA))
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === Std.( % ) fD fE) &&& (z5 === fF) &&& (z6 === fG) &&& isMove_conj__63 fD fA fB fC fF fE fG)
  and isMove_conj__63 z1 z2 z3 z4 z5 z6 z7 =
    fresh (fF fE fD fC fB fA)
      ( z1
      === left (s (o ()))
      &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD)
      &&& (z6 === Std.( % ) (fill ()) fE)
      &&& (z7 === fF) &&& goe__19 fA
      &&& eqNat_conj__27 fA fA (s fB) fC fE fD fF )
    ||| fresh (fG fF fE fD fC fB fA)
          ( z1
          === right (s fA)
          &&& (z2 === fB) &&& (z3 === fC) &&& (z4 === fD)
          &&& (z5 === s fE)
          &&& (z6 === fF) &&& (z7 === fG)
          &&& goe_conj__46 fE fA fB fA (s fC) fD fF fE fB fG )
  and isMove_conj__64 z1 z2 z3 z4 z5 z6 =
    fresh (fE fD fC fB fA)
      ( z1
      === left (s (o ()))
      &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC)
      &&& (z5 === Std.( % ) (fill ()) fD)
      &&& (z6 === fE) &&& goe__19 fA
      &&& eqNat_conj__27 fA fA (o ()) fB fD fC fE )
    ||| fresh (fF fE fD fC fB fA)
          ( z1
          === right (s fA)
          &&& (z2 === fB) &&& (z3 === fC)
          &&& (z4 === s fD)
          &&& (z5 === fE) &&& (z6 === fF)
          &&& goe_conj__46 fD fA fB fA (o ()) fC fE fD fB fF )
  and sub__65 z1 z2 z3 =
    fresh fA (z1 === o () &&& (z2 === fA) &&& (z3 === o ())) ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === fB) &&& (z3 === fC) &&& sub__66 fA fB fC)
  and sub__66 z1 z2 z3 =
    fresh fA (z1 === fA &&& (z2 === o ()) &&& (z3 === fA))
    ||| fresh fA (z1 === o () &&& (z2 === s fA) &&& (z3 === o ()))
    ||| fresh (fC fB fA) (z1 === s fA &&& (z2 === s fB) &&& (z3 === fC) &&& sub__66 fA fB fC)
  in
  checkAnswer y1 y2 y3 y4
