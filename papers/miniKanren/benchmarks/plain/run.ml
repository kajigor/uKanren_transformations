open OCanren
open GT
open Helper

let inputs =
  [ "original", Original.topLevel
  ; "  conspd", Conspd.topLevel
  ; "conspd_fresh", Conspd_fresh.topLevel
  ; "conspd_inline", Conspd_inline.topLevel
  ; "conspdEl", Conspd_elem.topLevel
  ; "     cpd", Cpd.topLevel
  ; " cpd_old", Cpd_old.topLevel
  ; "ecce_old", Ecce_old.topLevel
  ; "branches", Branch.topLevel
  ]


let _ =
  List.iter
    (fun (name, eval) ->
      run_formula 30 name @@
      run qrs (fun r t fm -> eval (Std.(%<) r t) fm)
    ) inputs


let _ =
  do_tables 5L 1000 (fun eval -> run q (fun fm -> fresh (q r) (eval (ocanren ([q;r])) fm))) inputs