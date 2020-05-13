open GT
open OCanren
open OCanren.Std


type bottle =
  | Fst
  | Snd
let fst_ () = !! Fst
let snd_ () = !! Snd
type stepType =
  | Fill
  | Empty
  | Pour
let fill () = !! Fill
let empty () = !! Empty
let pour () = !! Pour
type 'a0 gnat =
  | O
  | S of 'a0
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))