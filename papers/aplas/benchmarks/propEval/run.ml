open OCanren
open GT
open Helper

let inputs =
  [ "    ecceFirstPlain", EcceFirstPlain.topLevel
  ; "    ecceFirstNando", EcceFirstNando.topLevel
  ; "     ecceLastPlain", EcceLastPlain.topLevel
  ; "     ecceLastNando", EcceLastNando.topLevel
  ; " nonconjFirstPlain", NonconjFirstPlain.topLevel
  ; " nonconjFirstNando", NonconjFirstNando.topLevel
  ; "  nonconjLastPlain", NonconjLastPlain.topLevel
  ; "  nonconjLastNando", NonconjLastNando.topLevel
  ; "originalFirstPlain", OriginalFirstPlain.topLevel
  ; "originalFirstNando", OriginalFirstNando.topLevel
  ; " originalLastPlain", OriginalLastPlain.topLevel
  ; " originalLastNando", OriginalLastNando.topLevel
  ]

let _ =
  List.iter
    (fun (name, eval) ->
      run_formula 10 name @@
      run qrs (fun r t fm -> eval (Std.(%<) r t) fm)
    ) inputs


let _ =
  to_csv "res/results.csv" @@
  [do_tables 10L 1000 (fun eval -> run q (fun fm -> fresh (q r) (eval (ocanren ([q;r])) fm))) inputs "prop"]