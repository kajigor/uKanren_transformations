open GT

open OCanren
open OCanren.Std
open Tester

open Helper
open Bottles

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

(** For high order conversion **)
let checkAnswer q c n r = checkAnswer ((===) q) c ((===) n) r

let run_time n text r =
  let t = Sys.time() in
  let fx = RStream.take ~n:n @@ r (fun _ _ fm -> fm) in
  Printf.printf "%s: %fs\n" text (Sys.time() -. t);
  fx

let _ =
 (* run_exn myshow 1 q qh ("orig", fun q ->
    checkAnswer q capacities1 (int2nat 7) !!true
  )
  ;

  run_exn myshow 1 q qh ("trans", fun q ->
    Bottles_trans.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)
  )
  ;

  run_exn myshow 1 q qh ("defer", fun q ->
    Bottles_defer.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)
  )
  ;

  run_exn myshow 1 q qh ("calls", fun q ->
    Bottles_fun.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)
  )
  ; *)


  run_time 5 "trans" @@
  run q (fun q -> Bottles_trans.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7));

  run_time 5 "defer" @@
  run q (fun q -> Bottles_defer.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7));

  run_time 5 "orig" @@
  run q (fun q -> checkAnswer q capacities1 (int2nat 7) !!true);

  run_time 5 "calls" @@
  run q (fun q -> Bottles_fun.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7));

(*
  run_exn myshow 2 q qh ("cpd", fun q ->
    Cpd.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)); *)

  (* run_time 5 "cpd" @@
  run q (fun q -> Cpd.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)) *)



  run_exn myshow 2 q qh ("branches", fun q ->
    Branches.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7));

  run_time 5 "branches" @@
  run q (fun q -> Branches.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7))

