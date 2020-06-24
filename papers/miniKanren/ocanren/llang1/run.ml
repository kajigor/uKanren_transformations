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
      run q (fun fm -> transformer fm)
    )
    inputs

let _ =
  do_tables 100 (fun transformer -> run q (fun fm -> transformer fm)) inputs
