open OCanren
open GT
open Helper

let _ =
    (* run_formula 10 "original" @@
    run qrs (fun r t fm ->
      topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "original" @@
  run qrs (fun r t fm ->
    Original.topLevel (Std.(%<) r t) fm)

let _ =
  (* run_formula 30 "ecce" @@
  run qrs (fun r t fm ->
    topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "ecce" @@
  run qrs (fun r t fm ->
    Ecce.topLevel (Std.(%<) r t) fm)

let _ =
(*
  run_formula 30 "transformed" @@
  run qrs (fun r t fm ->
    topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "transformed" @@
  run qrs (fun r t fm ->
    Trans.topLevel (Std.(%<) r t) fm)
