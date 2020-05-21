open OCanren
open GT
open Helper

let _ =
    (* run_formula 10 "original" @@
    run qrs (fun r t fm ->
      topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "original 1000" @@
  run qrs (fun r t fm ->
    Original.topLevel (Std.(%<) r t) fm)

let _ =
  (* run_formula 30 "ecce" @@
  run qrs (fun r t fm ->
    topLevel (Std.(%<) r t) fm); *)

  run_time 10000 "ecce" @@
  run qrs (fun r t fm ->
    Ecce.topLevel (Std.(%<) r t) fm)

let _ =
(*
  run_formula 30 "transformed" @@
  run qrs (fun r t fm ->
    topLevel (Std.(%<) r t) fm); *)

  run_time 10000 "transformed" @@
  run qrs (fun r t fm ->
    Trans.topLevel (Std.(%<) r t) fm)

let _ =
    (* run_formula 30 "cpd" @@
    run qrs (fun r t fm ->
      Cpd.topLevel fm (Std.(%<) r t)); *)

    run_time 10000 "cpd" @@
    run qrs (fun r t fm ->
      Cpd.topLevel fm (Std.(%<) r t))

let _ =
  run_formula 30 "Branches" @@
  run qrs (fun r t fm ->
    Branches.topLevel fm (Std.(%<) r t));

  run_time 10000 "Branches" @@
  run qrs (fun r t fm ->
    Branches.topLevel fm (Std.(%<) r t))