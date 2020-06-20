open OCanren
open GT
open Helper

let inputList = ocanren ([0;1;2;3;4])

let _ =
  run_list 120 "Original\t" @@
  run q (fun q -> Original.topLevel q inputList)


let _ =
  run_list 120 "Ecce\t" @@
  run q (fun q -> Ecce.topLevel q inputList)


let _ =
  run_list 120 "Trans\t" @@
  run q (fun q -> Sort.topLevel q inputList)

