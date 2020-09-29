open OCanren
open GT
open Helper

let depth = Std.Nat.succ (Std.Nat.succ (Std.Nat.succ Std.Nat.zero))

let inputs =
  [ " originalLimited", OriginalLastPlainLimited.topLevel
  ; "   etalonLimited", EtalonLastPlainLimited.topLevel
  ; "        original", (fun x y z -> OriginalLastPlain.topLevel x y)
  ; "          etalon", (fun x y z -> EtalonLastPlain.topLevel x y)
  ]

let _ =
  List.iter
    (fun (name, eval) ->
      run_formula 100 name @@
      run qrs (fun r t fm -> eval (ocanren {[r;t]}) fm depth)
    ) inputs
(*
let _ =
  to_csv "res/results1.csv" @@
  [do_tables 3L 100 (fun eval -> run q (fun fm -> fresh (q) (eval (ocanren {[q]}) fm depth))) inputs "prop"]

let _ =
  to_csv "res/results2.csv" @@
  [do_tables 3L 100 (fun eval -> run q (fun fm -> fresh (q r) (eval (ocanren {[q; r]}) fm depth))) inputs "prop"] *)
