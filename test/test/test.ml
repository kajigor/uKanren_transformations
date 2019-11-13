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

let sortoTest = 
  (* let sorto_exp = !!! in *)
  let sorto_act = Sorto.sorto in 
  
  let run_test actual test testName = 
    Printf.printf "\nTest: %s\nSorting" testName ; 
    test actual in

  let run_test = run_test (* sorto_exp *) sorto_act in

  
  let test f = 
      run one (fun p -> sorto_act (nat_list [0;0;0;0;0]) p)
          (fun p ->
            Stream.take ~n:1 p |> List.iter (fun rr ->
              Printf.printf "%s\n%!"  @@ (if rr#is_open
              then
                GT.(show List.logic (show Nat.logic)) @@
                  rr#reify (List.reify Nat.reify)
              else
                GT.(show List.ground (show Nat.ground) rr#prj)
              )
            )
          )
    (* run q (fun q -> f q q)
                     (fun q -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:5 q) *)
  in run_test test "sorto [1,1,1,1] q" ;
  

(*  let test f = 
      run one (fun p -> sorto_act (nat_list [1;0;2;1;1]) p)
          (fun p ->
            Stream.take ~n:1 p |> List.iter (fun rr ->
              Printf.printf "%s\n%!"  @@ (if rr#is_open
              then
                GT.(show List.logic (show Nat.logic)) @@
                  rr#reify (List.reify Nat.reify)
              else
                GT.(show List.ground (show Nat.ground) rr#prj)
              )
            )
          )
    (* run q (fun q -> f q q)
                     (fun q -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:5 q) *)
  in run_test test "sorto [1,0,2,1,1] q" ;

  let test f = 
      run one (fun p -> sorto_act p p)
          (fun p ->
            Stream.take ~n:5 p |> List.iter (fun rr ->
              Printf.printf "%s\n%!"  @@ (if rr#is_open
              then
                GT.(show List.logic (show Nat.logic)) @@
                  rr#reify (List.reify Nat.reify)
              else
                GT.(show List.ground (show Nat.ground) rr#prj)
              )
            )
          )
    (* run q (fun q -> f q q)
                     (fun q -> List.iter (fun l -> Printf.printf "%s\n" @@ show_list @@ reify_list l) @@ Stream.take ~n:5 q) *)
  in run_test test "sorto q q"
*)

(* 
let reify_nat n = n#reify Nat.reify 
let show_nat n = show (Nat.logic) n

let minmaxo_test = 
  let minmaxo_act = Minmaxo.minmaxo in
  
  let run_test (* expected *) actual test testName = 
(*     Printf.printf "\nTest: %s\nExpected:\n" testName ; 
    test expected ;  *)
    Printf.printf "And here is what we've got :(\n" ; 
    test actual in

  let run_test = run_test minmaxo_act in
 
  let test f = run qrs (fun q r s -> f q s r r)
                       (fun q r s -> List.iter (fun l -> Printf.printf "%s\n" @@ show_nat @@ reify_nat) @@ Stream.take ~n:3 q) in
  run_test test "minmaxo q s r r" 
 *)   





(*

open GT
open MiniKanren
open Std
open MiniKanrenStd
open Minmaxo
open Nat

(* let (!) x = inj @@ lift x 

let reify_list l = l#reify @@ List.reify reify
let show_list  l = show(List.logic) (show(logic) (show int)) l
*) 
let reify_nat n = n#reify Nat.reify 
let show_nat n = show (Nat.logic) n

let run_test expected actual test testName = 
  Printf.printf "\nTest: %s\nExpected:\n" testName ; 
  test expected ; 
  Printf.printf "And here is what we've got :(\n" ; 
  test actual
(*
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
*)
let minmaxo_test = 
  run qrst (fun q r s t -> Minmaxo.minmaxo q r s t)
           (fun q r s t -> List.iter (fun (x, (y, (z, w))) -> Printf.printf "q: %s, s: %s, r: %s, w: %s\n\n" (show_nat (reify_nat x)) (show_nat (reify_nat y)) (show_nat (reify_nat z)) (show_nat (reify_nat w))) @@ Stream.take ~n:3 (Stream.zip q (Stream.zip r (Stream.zip s t))))

(*  let minmaxo_exp = Sort.minmaxo in 
  let minmaxo_act = Minmaxo.minmaxo in
  
  
   
  let run_test = run_test minmaxo_exp minmaxo_act in
  
   let test f = run qrs (fun q r s -> f q s r r)
                       (fun q r s -> List.iter (fun l -> Printf.printf "%s\n" @@ show_nat @@ reify_nat) @@ Stream.take ~n:3 q) in
  run_test test "minmaxo q s r r" 
  *)
  
  
  
  *)
