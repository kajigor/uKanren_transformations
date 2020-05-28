open OCanren
open GT
open Helper
open OCanren.Std


(* let _ =
  run_list 1 "original rep\t\t\t" @@
  run q (fun q -> Rep1.topLevel (ocanren (10)) q)

let _ =
  run_list 1 "rep unf 20, not normalized\t" @@
  run q (fun q -> Rep.topLevel (ocanren (10)) q)

let _ =
  run_list 10 "rep unf 50, normalized, extra vars" @@
  run q (fun q -> Rep50NormExtra.topLevel (ocanren (10)) q)

let _ =
  run_list 10 "rep unf 50,  normalized all the way" @@
  run q (fun q -> Rep50Norm.topLevel (ocanren (10)) q)

let _ =
  run_list 10 "rep unf 75,  normalized all the way" @@
  run q (fun q -> Rep75Norm.topLevel (ocanren (10)) q)

let _ =
  run_list 10 "rep unf 100, normalized all the way" @@
  run q (fun q -> Rep100.topLevel (ocanren (10)) q) *)



(* let _ =
  run_time 10 "original rep\t\t\t" @@
  run q (fun q -> Rep1.topLevel (ocanren (10)) q)


let _ =
  run_time 10 "rep unf 20,  not normalized\t" @@
  run q (fun q -> Rep20.topLevel (ocanren (10)) q)

let _ =
  run_time 10 "rep unf 50,  not normalized\t" @@
  run q (fun q -> Rep50.topLevel (ocanren (10)) q)

let _ =
  run_time 10 "rep unf 75,  not normalized\t" @@
  run q (fun q -> Rep75.topLevel (ocanren (10)) q)

let _ =
  run_time 10 "rep unf 100, not normalized\t" @@
  run q (fun q -> Rep100.topLevel (ocanren (10)) q)



let _ =
  run_time 10 "rep unf 50,  normalized all the way" @@
  run q (fun q -> Rep50Norm.topLevel (ocanren (10)) q)

let _ =
  run_time 10 "rep unf 75,  normalized all the way" @@
  run q (fun q -> Rep75Norm.topLevel (ocanren (10)) q)

let _ =
  run_time 10 "rep unf 100, normalized all the way" @@
  run q (fun q -> Rep100Norm.topLevel (ocanren (10)) q)


let _ =
  run_time 10 "rep unf 50, normalized, extra vars" @@
  run q (fun q -> Rep50NormExtra.topLevel (ocanren (10)) q) *)

let inputs = [ "original rep",  Rep1.topLevel
              ; "rep 20,  not", Rep20.topLevel
              ; "rep 50,  not", Rep50.topLevel
              ; "rep 75,  not", Rep75.topLevel
              ; "rep 100, not", Rep100.topLevel
              ; "rep 50,  all", Rep50Norm.topLevel
              ; "rep 75,  all", Rep75Norm.topLevel
              ; "rep 100, all", Rep100Norm.topLevel
              ; "rep 50, vars", Rep50NormExtra.topLevel
              ]

let _ =
  do_tables 10 (fun rel -> run q (fun q -> rel (ocanren (10)) q)) inputs

