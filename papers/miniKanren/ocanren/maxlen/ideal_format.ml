open GT
open OCanren
open OCanren.Std
open Helper

let topLevel y1 y2 y3 =
  let rec maxLengtho z1 z2 z3 =
    fresh (fC fB fA)
      (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& maxLengtho__1 fA fB fC)
  and maxLengtho__1 z1 z2 z3 =
    z1 === Std.List.nil () &&& (z2 === Std.Nat.zero) &&& (z3 === Std.Nat.zero)
    ||| fresh (fC fB fA)
          ( z1 === Std.( % ) Std.Nat.zero fA &&& (z2 === fB)
          &&& (z3 === Std.Nat.succ fC)
          &&& maxo1_conj__2 fA fB fC )
    ||| fresh (fD fC fB fA)
          ( z1
          === Std.( % ) (Std.Nat.succ fA) fB
          &&& (z2 === fC)
          &&& (z3 === Std.Nat.succ fD)
          &&& maxo1_conj__3 fB fA fC fD )
  and maxo1_conj__2 z1 z2 z3 =
    z1 === Std.List.nil () &&& (z2 === Std.Nat.zero) &&& (z3 === Std.Nat.zero)
    ||| fresh (fC fB fA)
          ( z1 === Std.( % ) Std.Nat.zero fA &&& (z2 === fB)
          &&& (z3 === Std.Nat.succ fC)
          &&& maxo1_conj__2 fA fB fC )
    ||| fresh (fD fC fB fA)
          ( z1 === Std.( % ) (Std.Nat.succ fA) fB
          &&& (z2 === fC)
          &&& (z3 === Std.Nat.succ fD)
          &&& maxo1_conj__3 fB fA fC fD )
  and maxo1_conj__3 z1 z2 z3 z4 =
    fresh fA
      ( z1 === Std.List.nil () &&& (z2 === fA)
      &&& (z3 === Std.Nat.succ fA)
      &&& (z4 === Std.Nat.zero) )
    ||| fresh (fE fD fC fB fA)
          ( z1 === Std.( % ) fA fB &&& (z2 === fC) &&& (z3 === fD)
          &&& (z4 === Std.Nat.succ fE)
          &&& leo_conj__4 fA fC fB fD fE )
    ||| fresh (fE fD fC fB fA)
          ( z1
          === Std.( % ) (Std.Nat.succ (Std.Nat.succ fA)) fB
          &&& (z2 === fC) &&& (z3 === fD)
          &&& (z4 === Std.Nat.succ fE)
          &&& gto__5 fA fC
          &&& maxo1_conj__3 fB (Std.Nat.succ fA) fD fE )
  and leo_conj__4 z1 z2 z3 z4 z5 =
    fresh (fD fC fB fA)
      ( z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC)
      &&& (z5 === fD) &&& maxo1_conj__3 fB fA fC fD )
    ||| fresh (fE fD fC fB fA)
          ( z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === fC)
          &&& (z4 === fD) &&& (z5 === fE) &&& leo__6 fA fB
          &&& maxo1_conj__3 fC fB fD fE )
  and gto__5 z1 z2 =
    fresh fA (z1 === fA &&& (z2 === Std.Nat.zero))
    ||| fresh (fB fA)
          (z1 === Std.Nat.succ fA &&& (z2 === Std.Nat.succ fB) &&& gto__5 fA fB)
  and leo__6 z1 z2 =
    fresh fA (z1 === Std.Nat.zero &&& (z2 === fA))
    ||| fresh (fB fA)
          (z1 === Std.Nat.succ fA &&& (z2 === Std.Nat.succ fB) &&& leo__6 fA fB)
  in
  maxLengtho y1 y2 y3