open OCanren
open GT
open Helper

let inputs =
  [ "original", Original.topLevel
  ; "  conspd", Conspd.topLevel
  (* ; "conspdor", Conspd_or_and.topLevel
  ; "conspd_fresh", Conspd_fresh.topLevel
  ; "conspd_inline", Conspd_inline.topLevel
  ; "conspdEl", Conspd_elem.topLevel *)
  ; "     cpd", Cpd.topLevel
  ; " cpd_new", Cpd_new.topLevel
  ; "branches", Branch.topLevel 
  ; "branches_new", Branch_new.topLevel 
  ]


let _ =
  List.iter
    (fun (name, eval) ->
      run_formula 30 name @@
      run qrs (fun r t fm -> eval (Std.(%<) r t) fm)
    ) inputs


let _ =
  do_tables 10L 1000 (fun eval -> run q (fun fm -> fresh (q r) (eval (Std.(%<) q r) fm))) inputs