module L = List

open GT
open OCanren
open OCanren.Std

@type 'a gnat =
  | O
  | S of 'a
  with show, gmap

@type ground_gnat = ground_gnat gnat with show, gmap
@type logic_gnat = logic_gnat gnat with show, gmap

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

(* module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0)) *)

module GNat = Fmap (struct
                     type 'a t = 'a gnat
                     let fmap f = gmap(gnat) f
                   end)

let o () = inj @@ GNat.distrib O
let s x  = inj @@ GNat.distrib (S x)


(******************************************)

let show_bottle = function
| Fst -> "1"
| Snd -> "2"

let show_stepType = function
| Fill  -> "F"
| Empty -> "E"
| Pour  -> "P"

let show_step = function
| (s, b) -> Printf.sprintf "%s%s" (show_bottle b) (show_stepType s)

let myshow x = show List.ground show_step x



(******************************************)

let rec int2nat n = if n = 0 then o () else s @@ int2nat @@ n - 1


let show_nat = show(logic_gnat)
let show_state = show(Std.Pair.ground) show_nat show_nat


let state x y = Std.Pair.pair (int2nat x) (int2nat y)
let cap x y = Std.Pair.pair (int2nat x) (int2nat y)
let step a b = Std.Pair.pair (!!a) (!!b)


let take n fn = fun goal -> RStream.take ~n:n @@ (fn goal) (fun fm -> fm)

let do_tables n fn lst =
  let samples = Benchmark.latencyN 5L (L.map (fun (name, goal) -> (name, take n fn, goal)) lst) in
  Benchmark.tabulate samples

let run_time n text r =
  let t = Sys.time() in
  let fx = RStream.take ~n:n @@ r (fun _ _ fm -> fm) in
  Printf.printf "%s: %fs\n" text (Sys.time() -. t);
  fx
