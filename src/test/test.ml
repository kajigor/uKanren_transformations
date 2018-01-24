open GT
open MiniKanren
open Std
open MiniKanrenStd

let (!) x = inj @@ lift x

let reify_list l = l#reify @@ List.reify reify
let show_list  l = show(List.logic) (show(logic) (show int)) l

let run_test expected actual test testName = 
  Printf.printf "\nTest: %s\nExpected:\n" testName ; 
  test expected ; 
  Printf.printf "And here is what we've got :(\n" ; 
  test actual

let doubleAppendoTest = 
  let appendo2_exp x y xy z xyz = List.appendo x y xy &&& List.appendo xy z xyz in 
  let appendo2_act = Appendo2.appendo2 in
  
  let run_test = run_test appendo2_exp appendo2_act in
  
  let test f = run qr (fun q r -> f (!< !1) (!< !2) r (!< !3) q)
                      (fun q r -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take q) in
  run_test test "appendo2 [1] [2] _ [3] q" ;

  let test f = run qr (fun q r -> f (!< !1) (!< !2) r (!< !3) q)
                      (fun q r -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take r) in
  run_test test "appendo2 [1] [2] q [3] _" ;
  
  let test f = run qr (fun q r -> f (!1 %< !5) (!< !2) r (!< !3) q)
                      (fun q r -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take q) in
  run_test test "appendo2 [1, 5] [2] _ [3] q" ;

  let test f = run qr (fun q r -> f (!< !1) (!2 %< !5) r (!< !3) q)
                      (fun q r -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take q) in
  run_test test "appendo2 [1] [2, 5] _ [3] q" ;

  let test f = run qrs (fun q r s -> f (!< !1) s r s q)
                       (fun q r s -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:3 q) in
  run_test test "appendo2 [1] s _ s q" ;

  let test f = run qrs (fun q r s -> f q s r s s)
                       (fun q r s -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:1 q) in
  run_test test "appendo2 q s _ s s" ;

  let test f = run qrs (fun q r s -> f s s r s q)
                       (fun q r s -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:3 q) in
  run_test test "appendo2 s s _ s q" 

(* Loops forever 
  let test f = run qr (fun q r -> f q (!< !2) r (!< !3) (!1 % (!2 %< !3)))
                       (fun q r -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take q) in
  runtest test "appendo2 q [2] _ [3] [1,2,3]"
  
*) 


let reversoTest = 
  let reverso_exp = List.reverso in 
  let reverso_act = Reverso.reverso in 

  let run_test expected actual test testName = 
    Printf.printf "\nTest: %s\nExpected:\n" testName ; 
    test expected ; 
    Printf.printf "And here is what we've got :(\n" ; 
    test actual in

  let run_test = run_test reverso_exp reverso_act in

  let test f = run q (fun q -> f (!1 %< !2) q)
                     (fun q -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:1 q)
  in
  run_test test "reverso [1,2] q" ;

  let test f = run q (fun q -> f q (!1 %< !2))
                     (fun q -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:1 q)
  in
  run_test test "reverso q [1,2]" ;

  let test f = run q (fun q -> f q q)
                     (fun q -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:5 q)
  in
  run_test test "reverso q q"




