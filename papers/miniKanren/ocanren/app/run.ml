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

let inputs =  [ "app1 ",  App1.topLevel
              ; "app15", App15.topLevel
              ]

let _ =
  do_tables 1 (fun app -> run q (fun q -> fresh (x y) ((q === Std.Pair.pair x y) &&& (app x y (ocanren ([1;2;3;4;5;6;7;8;9;10;11;12;13;14;15])))))) inputs