open GT
open MiniKanren
open Std
open Nat

let appendo2 x0 x1 x2 x3 x4 =
  let rec f0 x0 x1 x2 x3 x4 =
    fresh (x7 x6 x5 x10 x9 x8)
      ( x0 === nil () &&& (x2 === x1)
      &&& ( x1 === nil () &&& (x4 === x3)
          ||| ( x1 === x8 % x9
              &&& (x4 === x8 % x10)
              &&&
              let rec f1 x9 x3 x10 = fresh (x13 x12 x11) (x9 === nil () &&& (x10 === x3) ||| (x9 === x11 % x12 &&& (x10 === x11 % x13) &&& f1 x12 x3 x13)) in
              defer (f1 x9 x3 x10) ) )
      ||| (x0 === x5 % x6 &&& (x2 === x5 % x7) &&& (x5 === x8) &&& (x7 === x9) &&& (x4 === x8 % x10) &&& f0 x6 x1 x9 x3 x10) )
  in
  defer (f0 x0 x1 x2 x3 x4)
