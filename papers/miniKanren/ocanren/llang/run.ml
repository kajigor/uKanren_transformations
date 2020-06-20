open OCanren
open GT
open Helper

let _ =
  run_formula 10 "original" @@
  run qr (fun g fm ->
    Original.topLevel fm g (integer ()))
