open GT

open OCanren
open OCanren.Std
open Tester

open Helper
open Bottles

(** For high order conversion **)
let checkAnswer q c n r = checkAnswer ((===) q) c ((===) n) r

(* let _ =
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

(*
  run_exn myshow 2 q qh ("cpd", fun q ->
    Cpd.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)); *)

  (* run_time 5 "cpd" @@
  run q (fun q -> Cpd.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)) *)



  (* run_exn myshow 2 q qh ("branches", fun q ->
    Branches.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7));

  run_time 5 "branches" @@
  run q (fun q -> Branches.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)) *)





  run_time 5 "trans" @@
  run q (fun q -> Bottles_trans.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7));

  run_time 5 "defer" @@
  run q (fun q -> Bottles_defer.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7));

  run_time 5 "orig" @@
  run q (fun q -> checkAnswer q capacities1 (int2nat 7) !!true);

  run_time 5 "calls" @@
  run q (fun q -> Bottles_fun.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7)) *)



let inputs = [ "trans", Bottles_trans.topLevel
             ; "defer", Bottles_defer.topLevel
             ; "calls", Bottles_fun.topLevel
             ; "orig ", (fun q _ _ -> checkAnswer q capacities1 (int2nat 7) !!true)
             ; "noHO" , Original_no_ho.topLevel
             ; "different", Different.topLevel
             ; "ecce" , Ecce.topLevel
             ]

let _ =
  run_exn myshow 1 q qh ("original", fun q ->
    Ecce.topLevel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7))

let _ =
  do_tables 2 (fun rel -> run q (fun q -> rel q (Std.Pair.pair (int2nat 4) (int2nat 9)) (int2nat 7))) inputs
