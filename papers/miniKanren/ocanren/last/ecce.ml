open OCanren
open GT
open Helper

let topLevel y1 y2 =
  let rec eval z1 z2 z3 = fresh (fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === !!true) &&& eval__1 fA fB)
  and eval__1 z1 z2 =
    fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!true)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& eval__1 fA fB &&& eval__1 fA fC)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& eval_conj__3 fA fB fC)
    ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__4 fA fB)
  and elem__2 z1 z2 z3 z4 =
    fresh (fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fA))
    ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem__2 fA fC fD fE)
  and eval_conj__3 z1 z2 z3 =
    fresh (fG fF fE fD fC fB fA)
      (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& elem__2 fC fA fB fE &&& eval__13 fA fB fD fF &&& nand__8 fE fG &&& nand_conj__14 fF fG)
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand_conj__12 fE fF fG &&& eval__6 fA fD fH &&& nand__8 fG fI &&& nand_conj__14 fH fI
          )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand__8 fE fG &&& nand_conj__9 fF fG fH &&& eval__6 fA fD fI &&& nand__8 fH fJ
          &&& nand_conj__14 fI fJ )
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& eval_conj__15 fA fB fC fD fE &&& nand__16 fD fE)
  and eval__4 z1 z2 =
    fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!false)
    ||| fresh (fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& eval_conj__5 fA fB fD fC fD)
    ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& eval__4 fA fB &&& eval__4 fA fC)
    ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__1 fA fB)
  and eval_conj__5 z1 z2 z3 z4 z5 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === Std.( % ) fA fB
      &&& (z2 === var fC)
      &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem__2 fC fA fB fD &&& eval__13 fA fB fE fG &&& nand__7 fF fG !!true )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand_conj__12 fG fH fD &&& eval__6 fA fE fI
          &&& nand__7 fF fI !!true )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fD
          &&& eval__6 fA fE fJ &&& nand__7 fF fJ !!true )
    ||| fresh (fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval__6 fA fB fF &&& nand__8 fC fF &&& eval__6 fA fD fG
          &&& nand__7 fE fG !!true )
  and eval__6 z1 z2 z3 =
    fresh (fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& elem__2 fC fA fB fD)
    ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand_conj__12 fE fF fD)
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === disj fB fC) &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand__8 fE fG &&& nand_conj__9 fF fG fD)
    ||| fresh (fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& eval__6 fA fB fD &&& nand__8 fC fD)
  and nand__7 z1 z2 z3 =
    z1 === !!false &&& (z2 === !!false) &&& (z3 === !!true)
    ||| (z1 === !!false &&& (z2 === !!true) &&& (z3 === !!true))
    ||| (z1 === !!true &&& (z2 === !!false) &&& (z3 === !!true))
    ||| (z1 === !!true &&& (z2 === !!true) &&& (z3 === !!false))
  and nand__8 z1 z2 = z1 === !!false &&& (z2 === !!true) ||| (z1 === !!true &&& (z2 === !!false))
  and nand_conj__9 z1 z2 z3 =
    fresh (fB fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === fB) &&& nand__10 fA fB)
    ||| fresh (fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === !!true) &&& nand__11 fA)
  and nand__10 z1 z2 = z1 === !!false &&& (z2 === !!true) ||| (z1 === !!true &&& (z2 === !!false))
  and nand__11 z1 = z1 === !!false ||| (z1 === !!true)
  and nand_conj__12 z1 z2 z3 =
    z1 === !!false &&& (z2 === !!false) &&& (z3 === !!false)
    ||| (z1 === !!false &&& (z2 === !!true) &&& (z3 === !!false))
    ||| (z1 === !!true &&& (z2 === !!false) &&& (z3 === !!false))
    ||| (z1 === !!true &&& (z2 === !!true) &&& (z3 === !!true))
  and eval__13 z1 z2 z3 z4 =
    fresh (fD fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === var fC) &&& (z4 === fD) &&& elem__2 fC fA fB fD)
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === fB) &&& (z3 === conj fC fD) &&& (z4 === fE) &&& eval__13 fA fB fC fF &&& eval__13 fA fB fD fG &&& nand_conj__12 fF fG fE)
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA &&& (z2 === fB)
          &&& (z3 === disj fC fD)
          &&& (z4 === fE) &&& eval__13 fA fB fC fF &&& eval__13 fA fB fD fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fE )
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === neg fC) &&& (z4 === fD) &&& eval__13 fA fB fC fE &&& nand__8 fD fE)
  and nand_conj__14 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| fresh (fA) (z1 === !!true &&& (z2 === fA) &&& nand__11 fA)
  and eval_conj__15 z1 z2 z3 z4 z5 =
    fresh (fH fG fF fE fD fC fB fA)
      ( z1 === Std.( % ) fA fB
      &&& (z2 === var fC)
      &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__28 fC fA fB fG &&& eval__13 fA fB fD fH &&& nand__8 fG fE &&& nand__8 fH fF )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__21 fA fB fC fG &&& eval__6 fA fD fH &&& nand__8 fG fE &&& nand__8 fH fF )
    ||| fresh (fL fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
          &&& nand__8 fK fJ &&& eval__6 fA fD fL &&& nand__8 fK fE &&& nand__8 fL fF )
    ||| fresh (fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__17 fA fB fC fF fD &&& nand__8 fF fE)
  and nand__16 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| (z1 === !!false &&& (z2 === !!true)) ||| (z1 === !!true &&& (z2 === !!false))
  and eval_conj__17 z1 z2 z3 z4 z5 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === Std.( % ) fA fB
      &&& (z2 === var fC)
      &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__27 fC fA fB fG &&& eval__13 fA fB fD fE &&& nand__8 fG fF )
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__21 fA fB fC fG &&& nand__8 fH fG &&& eval__6 fA fD fE &&& nand__8 fH fF )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
          &&& nand_conj__22 fJ fK &&& eval__6 fA fD fE &&& nand__8 fK fF )
    ||| fresh (fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__18 fA fB fF fC fD &&& nand__8 fF fE)
  and eval_conj__18 z1 z2 z3 z4 z5 =
    fresh (fF fE fD fC fB fA)
      (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__20 fC fA fB fD &&& eval__13 fA fB fE fF)
    ||| fresh (fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__21 fA fB fC fG &&& nand_conj__22 fG fD &&& eval__6 fA fE fF )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
          &&& nand_conj__23 fJ fD &&& eval__6 fA fE fF )
    ||| fresh (fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__19 fA fB fF &&& nand__8 fC fF &&& eval__6 fA fD fE)
  and eval_conj__19 z1 z2 z3 =
    fresh (fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& elem_conj__20 fC fA fB fD)
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& eval_conj__21 fA fB fC fE &&& nand_conj__22 fE fD)
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand__8 fE fG &&& nand_conj__9 fF fG fH &&& nand_conj__23 fH fD )
    ||| fresh (fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& eval_conj__19 fA fB fD &&& nand__8 fC fD)
  and elem_conj__20 z1 z2 z3 z4 =
    fresh (fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& nand_conj__23 fA fC)
    ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem_conj__20 fA fC fD fE)
  and eval_conj__21 z1 z2 z3 z4 =
    fresh (fG fF fE fD fC fB fA)
      (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem__2 fC fA fB fF &&& eval__13 fA fB fD fG &&& nand_conj__32 fF fG fE)
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand_conj__12 fF fG fH &&& eval_conj__24 fA fD fH fE )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& eval_conj__24 fA fD fI fE
          )
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__25 fA fB fC fE &&& nand__8 fD fE)
  and nand_conj__22 z1 z2 = z1 === !!true &&& (z2 === !!true) ||| (z1 === !!false &&& (z2 === !!false))
  and nand_conj__23 z1 z2 = z1 === !!true &&& (z2 === !!false) ||| (z1 === !!false &&& (z2 === !!true))
  and eval_conj__24 z1 z2 z3 z4 =
    fresh (fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__29 fC fA fB fD fE)
    ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__30 fA fB fC fD fF &&& nand_conj__31 fF fE)
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__32 fD fI fE )
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__33 fA fB fC fE &&& nand__8 fD fE)
  and eval_conj__25 z1 z2 z3 z4 =
    fresh (fG fF fE fD fC fB fA)
      ( z1 === Std.( % ) fA fB
      &&& (z2 === var fC)
      &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__28 fC fA fB fF &&& eval__13 fA fB fD fG &&& nand_conj__12 fF fG fE )
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& eval__6 fA fD fG &&& nand_conj__12 fF fG fE)
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand__8 fJ fI
          &&& eval__6 fA fD fK &&& nand_conj__12 fJ fK fE )
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__26 fA fB fC fE &&& nand__8 fE fD)
  and eval_conj__26 z1 z2 z3 z4 =
    fresh (fG fF fE fD fC fB fA)
      (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__27 fC fA fB fF &&& eval__13 fA fB fD fG &&& nand__7 fF fG fE)
    ||| fresh (fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& nand__8 fG fF &&& eval__6 fA fD fH &&& nand__7 fG fH fE )
    ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__22 fI fJ
          &&& eval__6 fA fD fK &&& nand__7 fJ fK fE )
    ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__18 fA fB fE fC fF &&& nand__7 fE fF fD)
  and elem_conj__27 z1 z2 z3 z4 =
    fresh (fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& nand_conj__22 fA fC)
    ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem_conj__27 fA fC fD fE)
  and elem_conj__28 z1 z2 z3 z4 =
    fresh (fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& nand__8 fC fA)
    ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem_conj__28 fA fC fD fE)
  and elem_conj__29 z1 z2 z3 z4 z5 =
    fresh (fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& nand_conj__32 fC fA fD)
    ||| fresh (fF fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__29 fA fC fD fE fF)
  and eval_conj__30 z1 z2 z3 z4 z5 =
    fresh (fJ fI fH fG fF fE fD fC fB fA)
      ( z1 === Std.( % ) fA fB
      &&& (z2 === var fC)
      &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem__2 fC fA fB fG &&& eval__13 fA fB fD fH &&& nand__7 fG fH fI &&& nand__8 fI fJ
      &&& nand__7 fE fJ fF )
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === conj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand_conj__12 fG fH fI &&& eval_conj__41 fA fD fI fE fF
          )
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
          &&& eval_conj__41 fA fD fJ fE fF )
    ||| fresh (fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__25 fA fB fC fF &&& nand__7 fD fF fE)
  and nand_conj__31 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| (z1 === !!true &&& (z2 === !!true))
  and nand_conj__32 z1 z2 z3 =
    z1 === !!false &&& (z2 === !!false) &&& (z3 === !!true)
    ||| (z1 === !!false &&& (z2 === !!true) &&& (z3 === !!true))
    ||| (z1 === !!true &&& (z2 === !!false) &&& (z3 === !!true))
    ||| (z1 === !!true &&& (z2 === !!true) &&& (z3 === !!false))
  and eval_conj__33 z1 z2 z3 z4 =
    fresh (fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__34 fC fA fB fD fE)
    ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& nand_conj__12 fD fF fE)
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__35 fI fD fE )
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__36 fA fB fC fE &&& nand__8 fE fD)
  and elem_conj__34 z1 z2 z3 z4 z5 =
    fresh (fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& nand_conj__35 fA fC fD)
    ||| fresh (fF fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__34 fA fC fD fE fF)
  and nand_conj__35 z1 z2 z3 =
    fresh (fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === !!false) &&& nand__11 fA)
    ||| fresh (fB fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === fB) &&& nand_conj__40 fA fB)
  and eval_conj__36 z1 z2 z3 z4 =
    fresh (fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__37 fC fA fB fD fE)
    ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& nand_conj__38 fF fD fE)
    ||| fresh (fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__39 fI fD fE )
    ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__19 fA fB fE &&& nand__7 fC fE fD)
  and elem_conj__37 z1 z2 z3 z4 z5 =
    fresh (fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& nand_conj__39 fA fC fD)
    ||| fresh (fF fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__37 fA fC fD fE fF)
  and nand_conj__38 z1 z2 z3 =
    fresh (fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === !!true) &&& nand__11 fA)
    ||| fresh (fB fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === fB) &&& nand__10 fA fB)
  and nand_conj__39 z1 z2 z3 =
    fresh (fB fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === fB) &&& nand__10 fA fB)
    ||| fresh (fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === !!true) &&& nand__11 fA)
  and nand_conj__40 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| (z1 === !!true &&& (z2 === !!true))
  and eval_conj__41 z1 z2 z3 z4 z5 =
    fresh (fF fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__42 fC fA fB fD fE fF)
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__30 fA fB fC fD fG &&& nand_conj__9 fG fE fF)
    ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === fA
          &&& (z2 === disj fB fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
          &&& nand_conj__43 fD fJ fE fF )
    ||| fresh (fF fE fD fC fB fA)
          (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__33 fA fB fC fF &&& nand__7 fD fF fE)
  and elem_conj__42 z1 z2 z3 z4 z5 z6 =
    fresh (fE fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& nand_conj__43 fC fA fD fE)
    ||| fresh (fG fF fE fD fC fB fA)
          (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fG) &&& elem_conj__42 fA fC fD fE fF fG)
  and nand_conj__43 z1 z2 z3 z4 =
    fresh (fA) (z1 === !!false &&& (z2 === !!false) &&& (z3 === fA) &&& (z4 === !!true) &&& nand__11 fA)
    ||| fresh (fA) (z1 === !!false &&& (z2 === !!true) &&& (z3 === fA) &&& (z4 === !!true) &&& nand__11 fA)
    ||| fresh (fA) (z1 === !!true &&& (z2 === !!false) &&& (z3 === fA) &&& (z4 === !!true) &&& nand__11 fA)
    ||| fresh (fB fA) (z1 === !!true &&& (z2 === !!true) &&& (z3 === fA) &&& (z4 === fB) &&& nand__10 fA fB)
  in
  eval__1 y1 y2