open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec fail () = fail 
  and notZero x = (fresh (y) ((x === (Nat.succ y)))) 
  and addo x y z = (((x === Nat.zero) &&& (y === z)) ||| (fresh (x' z') (((x === (Nat.succ x')) &&& (z === (Nat.succ z')) &&& (addo x' y z'))))) 
  and mulo x y z = (((x === Nat.zero) &&& (z === Nat.zero)) ||| (fresh (x' z') (((x === (Nat.succ x')) &&& (addo y z' z) &&& (mulo x' y z'))))) 
  and leo x y b = (((x === Nat.zero) &&& (b === (trueo ()))) ||| (fresh (zz) (((x === (Nat.succ zz)) &&& (y === Nat.zero) &&& (b === (falso ()))))) ||| (fresh (x' y') (((x === (Nat.succ x')) &&& (y === (Nat.succ y')) &&& (leo x' y' b))))) 
  and gto x y b = ((fresh (zz) (((x === (Nat.succ zz)) &&& (y === Nat.zero) &&& (b === (trueo ()))))) ||| ((x === Nat.zero) &&& (b === (falso ()))) ||| (fresh (x' y') (((x === (Nat.succ x')) &&& (y === (Nat.succ y')) &&& (gto x' y' b))))) 
  and geo x y z = (leo y x z) 
  and lto x y z = (gto y x z) 
  in         (fresh (y z) ((addo ((Nat.succ ((Nat.succ Nat.zero)))) y z)))
