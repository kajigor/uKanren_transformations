open OCanren
open GT
open Helper

let inputList = ocanren ([1;2;3;4;5;6;7;8;9;10;11;12;13;14;15])

let _ =
  run_2_nums 2 "nonconj\t" @@
  run q (fun q -> fresh (x y) ((q === Std.Pair.pair x y) &&& (Maxlen.topLevel inputList x y)))

let _ =
  run_2_nums 2 "original\t" @@
  run q (fun q -> fresh (x y) ((q === Std.Pair.pair x y) &&& (Original.topLevel inputList x y)))

let _ =
  run_2_nums 2 "ideal\t" @@
  run q (fun q -> fresh (x y) ((q === Std.Pair.pair x y) &&& (Ideal.topLevel inputList x y)))

let inputs =  [ "nonconj", Maxlen.topLevel
              ; "orignal", Original.topLevel
              ; "ideal",   Ideal.topLevel
              ]


let _ =
  do_tables 1 (fun app -> run q (fun q -> fresh (x y) ((q === Std.Pair.pair x y) &&& (app inputList x y)))) inputs