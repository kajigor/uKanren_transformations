open OCanren
open GT
open Helper

let inputs =
  [
    "     ecceLastPlain", EcceLastPlain.topLevel
  ; "  nonconjLastPlain", NonconjLastPlain.topLevel
  ; " originalLastPlain", OriginalLastPlain.topLevel
  (*; "    geoffLastPlain", Geoff_originalLastPlain.topLevel *)
  ; "   etalonLastPlain", EtalonLastPlain.topLevel
  ; "   mergedLastPlain", MergedLastPlain.topLevel
  ]

  (* [
    "             geoff", Geoff.topLevel
  ; "      geoffReverse", Geoff_reverse.topLevel

  ; "    ecceFirstPlain", EcceFirstPlain.topLevel
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
  ; "   geoffFirstPlain", Geoff_originalFirstPlain.topLevel
  ; "   geoffFirstNando", Geoff_originalFirstNando.topLevel
  ; "    geoffLastPlain", Geoff_originalLastPlain.topLevel
  ; "    geoffLastNando", Geoff_originalLastNando.topLevel
  ] *)

let _ =
  List.iter
    (fun (name, eval) ->
      run_formula 5 name @@
      run qrs (fun r t fm -> eval (Std.(%<) r t) fm &&& depth fm (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ Std.Nat.zero)))))))
    ) inputs

let _ =
  to_csv "res/results.csv" @@
  [do_tables 10L 10 (fun eval -> run q (fun fm -> fresh (q r) (eval (ocanren {[q;r]}) fm &&& depth fm (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ (Std.Nat.succ Std.Nat.zero))))))))) inputs "prop"]
