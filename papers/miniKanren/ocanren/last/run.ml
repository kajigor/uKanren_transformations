open OCanren
open GT
open Helper

let _ =
  run_time 1000 "ecce" @@
  run qrs (fun r t fm ->
    Ecce.topLevel (Std.(%<) r t) fm)

let _ =
  run_time 1000 "original" @@
  run qrs (fun r t fm ->
    Original.topLevel (Std.(%<) r t) fm)

let _ =
  run_time 1000 "original: fresh" @@
  run qrs (fun r t fm ->
    Original_fresh.topLevel (Std.(%<) r t) fm)

let _ =
  (* run_formula 5 "transformed" @@
  run qrs (fun r t fm ->
    Trans.topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "transformed original" @@
  run qrs (fun r t fm ->
    Trans.topLevel (Std.(%<) r t) fm)

let _ =
  (* run_formula 5 "transformed" @@
  run qrs (fun r t fm ->
    Trans_cfa.topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "transformed: close fresh auto" @@
  run qrs (fun r t fm ->
    Trans_cfa.topLevel (Std.(%<) r t) fm)


let _ =
  (* run_formula 5 "transformed" @@
  run qrs (fun r t fm ->
    Trans_bottles.topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "transformed: bottle" @@
  run qrs (fun r t fm ->
    Trans_bottles.topLevel (Std.(%<) r t) fm)


let _ =
  (* run_formula 5 "transformed" @@
  run qrs (fun r t fm ->
    Trans_cfnp.topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "transformed: close fresh, no purification" @@
  run qrs (fun r t fm ->
    Trans_cfnp.topLevel (Std.(%<) r t) fm)



let _ =
  (* run_formula 5 "transformed" @@
  run qrs (fun r t fm ->
    Trans_cfhu.topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "transformed: close fresh, extra unifs removed by hand" @@
  run qrs (fun r t fm ->
    Trans_cfhu.topLevel (Std.(%<) r t) fm)


let _ =
  (* run_formula 5 "global" @@
  run qrs (fun r t fm ->
    Trans_global.topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "global" @@
  run qrs (fun r t fm ->
    Trans_global.topLevel (Std.(%<) r t) fm)


let _ =
  (* run_formula 5 "defer" @@
  run qrs (fun r t fm ->
    Trans_defer.topLevel (Std.(%<) r t) fm); *)

  run_time 1000 "defer" @@
  run qrs (fun r t fm ->
    Trans_defer.topLevel (Std.(%<) r t) fm)

