open OCanren
open GT
open Helper

let inputs =
  [ "original   ", Original.topLevel
  ; "ecce       ", Ecce.topLevel
  ; "transformed", Trans.topLevel
  ; "cpd        ", Cpd.topLevel
  ; "branches   ", Branches.topLevel
  ]

(* 
let _ =
  List.iter
    (fun (name, eval) ->
      run_formula 30 name @@
      run qrs (fun r t fm -> eval (Std.(%<) r t) fm)
    ) inputs
*)

let _ =
  do_tables 1000 (fun eval -> run q (fun fm -> fresh (q r) (eval (ocanren ([q;r])) fm))) inputs