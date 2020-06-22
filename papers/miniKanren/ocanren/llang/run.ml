open OCanren
open GT
open Helper

let inputs = [ ("original", Original.topLevel)
             ; ("trans", Llang.topLevel)
             ; ("ecce", Ecce.topLevel)
             ]

let _ =
  List.map
    (fun (name, transformer) ->
      run_formula 100 name @@
      run qr (fun fm g ->
        (g === (ocanren([])) &&& transformer fm g))
    )
    inputs

let _ =
  do_tables 100 (fun transformer -> run q (fun fm -> transformer fm (ocanren([])))) inputs



(* let _ =
  List.map
    (fun (name, transformer) ->
      run_formula 1000 name @@
      run qr (fun fm g ->
        fresh (x1 x2 x3) (g === (ocanren([x1; x2; x3])) &&& transformer fm g))
    )
    inputs *)

(* let _ =
  List.map
    (fun (name, transformer) ->
      run_formula 1000   name @@
      run qr (fun fm g ->
        (g === Std.(%<) (integer ()) (boolean ()) &&& transformer fm g))
    )
    inputs *)
