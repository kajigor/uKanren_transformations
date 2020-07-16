module L = List

open OCanren
open OCanren.Std
open GT
open Helper

let rec int2nat n = if n <= 0 then Nat.zero else Nat.succ (int2nat (n - 1))
let v n = var_ (int2nat n)
let c n a = constr (int2nat n) @@ List.list a

let t1 :
((OCanren.Std.Nat.ground, Helper.ground_term,
  (OCanren.Std.Nat.ground, Helper.ground_term,
   Helper.ground_term OCanren.Std.List.ground)
  Helper.aterm OCanren.Std.List.ground)
 Helper.aterm,
 (OCanren.Std.Nat.logic, Helper.logic_term,
  (OCanren.Std.Nat.logic, Helper.logic_term,
   ((OCanren.Std.Nat.logic, Helper.logic_term,
     'a OCanren.logic OCanren.Std.List.logic)
    Helper.aterm as 'a)
   OCanren.logic OCanren.Std.List.logic)
  Helper.aterm OCanren.logic OCanren.Std.List.logic)
 Helper.aterm OCanren.logic)
OCanren.injected = c 0 [v 0; c 1 []]
let t1' = c 0 [c 1 []; v 0]

let ((t2 :
((OCanren.Std.Nat.ground, Helper.ground_term,
  (OCanren.Std.Nat.ground, Helper.ground_term,
   (OCanren.Std.Nat.ground, Helper.ground_term,
    (OCanren.Std.Nat.ground, Helper.ground_term,
     Helper.ground_term OCanren.Std.List.ground)
    Helper.aterm OCanren.Std.List.ground)
   Helper.aterm OCanren.Std.List.ground)
  Helper.aterm OCanren.Std.List.ground)
 Helper.aterm,
 (OCanren.Std.Nat.logic, Helper.logic_term,
  (OCanren.Std.Nat.logic, Helper.logic_term,
   (OCanren.Std.Nat.logic, Helper.logic_term,
    (OCanren.Std.Nat.logic, Helper.logic_term,
     ((OCanren.Std.Nat.logic, Helper.logic_term,
       'a OCanren.logic OCanren.Std.List.logic)
      Helper.aterm as 'a)
     OCanren.logic OCanren.Std.List.logic)
    Helper.aterm OCanren.logic OCanren.Std.List.logic)
   Helper.aterm OCanren.logic OCanren.Std.List.logic)
  Helper.aterm OCanren.logic OCanren.Std.List.logic)
 Helper.aterm OCanren.logic)
OCanren.injected)
, t2') =
  let appendo x y z = c 1 [x; y; z] in
  let cons x y = c 2 [x; y] in
  let nil = c 3 [] in
  let list2 x y = cons x (cons y nil) in
  let cA = c 4 [] in
  let cB = c 5 [] in
  let cC = c 6 [] in
  let cD = c 7 [] in
  let vX = v 0 in
  let vXs = v 1 in
  let vYs = v 2 in
  let vZs = v 3 in
  let vLs = v 4 in
  appendo (list2 cA cB) (list2 cC cD) (vLs),        (* append([a, b]  , [c, d], Ls      ) *)
  appendo (cons vX vXs) (vYs)         (cons vX vZs) (* append([X | Xs], Ys    , [X | Zs]) *)

let ((t3  :
((OCanren.Std.Nat.ground, Helper.ground_term,
  (OCanren.Std.Nat.ground, Helper.ground_term,
   (OCanren.Std.Nat.ground, Helper.ground_term,
    Helper.ground_term OCanren.Std.List.ground)
   Helper.aterm OCanren.Std.List.ground)
  Helper.aterm OCanren.Std.List.ground)
 Helper.aterm,
 (OCanren.Std.Nat.logic, Helper.logic_term,
  (OCanren.Std.Nat.logic, Helper.logic_term,
   (OCanren.Std.Nat.logic, Helper.logic_term,
    ((OCanren.Std.Nat.logic, Helper.logic_term,
      'a OCanren.logic OCanren.Std.List.logic)
     Helper.aterm as 'a)
    OCanren.logic OCanren.Std.List.logic)
   Helper.aterm OCanren.logic OCanren.Std.List.logic)
  Helper.aterm OCanren.logic OCanren.Std.List.logic)
 Helper.aterm OCanren.logic)
OCanren.injected), t3') =
  let f x y z = c 0 [x; y; z] in
  let g x y   = c 1 [x; y] in
  let p       = c 2 [] in
  let t       = c 3 [] in
  let x       = v 0 in
  let y       = v 1 in
  let l       = v 2 in
  let z       = v 3 in
  f x x (g z t),
  f (g p l) y y

let inputs =
  [ " original", Original.topLevel
  ; " branches", Branch.topLevel
  ; "      cpd", Cpd.topLevel
  ; "   conspd", Conspd.topLevel
  ]

(* let _ =
  run_subst 1 "trans1" @@
  run q (fun q -> Trans.topLevel q t1 t1')
  ;

  run_subst 3 "trans2" @@
  run q (fun q -> Trans.topLevel q t2 t2')
  ;

  run_subst 3 "trans3" @@
  run q (fun q -> Trans.topLevel q t3 t3')

let _ =
  run_subst 1 "ecce" @@
  run q (fun q -> Cpd.topLevel q t1 t1')
  ;

  run_subst 3 "ecce2" @@
  run q (fun q -> Cpd.topLevel q t2 t2')
  ;

  run_subst 3 "ecce3" @@
  run q (fun q -> Cpd.topLevel q t3 t3') *)

let _ =
  do_tables 10000L 1 (fun unifier -> run q (fun q -> unifier q t1 t1')) inputs ;
  do_tables 100L 3 (fun unifier -> run q (fun q -> unifier q t2 t2')) (L.tl inputs) ;
  do_tables 10L 3 (fun unifier -> run q (fun q -> unifier q t3 t3')) (L.tl @@ L.tl inputs)

(*
let _ =
  (* run_formula 5 "original" @@
  run qrs (fun q r s -> Original.check_uni q r s !!true) *)
  run_subst 1 "original1" @@
  run q (fun q -> Original.check_uni q t1 t1' !!true)
  (* ;

  run_subst 1 "original" @@
  run q (fun q -> Original.check_uni q t2 t2' !!true);

  run_subst 1 "original" @@
  run q (fun q -> Original.check_uni q t3 t3' !!true) *)

let _ =
  (* run_formula 5 "spec" @@
  run qrs (fun q r s -> Spec.topLevel q r s) *)
  (* run_subst 1 "spec1" @@
  run q (fun q -> Spec.topLevel q t1 t1')
  ;

  run_subst 3 "spec2" @@
  run q (fun q -> Spec.topLevel q t2 t2')
  ;

  run_subst 3 "spec3" @@
  run q (fun q -> Spec.topLevel q t3 t3') *)

  run_time 1 "spec1" @@
  run q (fun q -> Spec.topLevel q t1 t1')
  ;

  run_time 3 "spec2" @@
  run q (fun q -> Spec.topLevel q t2 t2')
  ;

  run_time 3 "spec3" @@
  run q (fun q -> Spec.topLevel q t3 t3')



let _ =
  (* run_formula 5 "spec" @@
  run qrs (fun q r s -> Spec.topLevel q r s) *)
  run_subst 1 "trans1" @@
  run q (fun q -> Trans.topLevel q t1 t1')
  ;

  run_subst 3 "trans2" @@
  run q (fun q -> Trans.topLevel q t2 t2')
  ;

  run_subst 3 "trans3" @@
  run q (fun q -> Trans.topLevel q t3 t3');

  run_time 1 "trans1" @@
  run q (fun q -> Trans.topLevel q t1 t1')
  ;

  run_time 3 "trans2" @@
  run q (fun q -> Trans.topLevel q t2 t2')
  ;

  run_time 3 "trans3" @@
  run q (fun q -> Trans.topLevel q t3 t3')

let _ =
  (* run_formula 5 "spec" @@
  run qrs (fun q r s -> Spec.topLevel q r s) *)
  (* run_subst 1 "transUnf1" @@
  run q (fun q -> Trans_unf.topLevel q t1 t1')
  ;

  run_subst 3 "transUnf2" @@
  run q (fun q -> Trans_unf.topLevel q t2 t2')
  ;

  run_subst 3 "transUnf3" @@
  run q (fun q -> Trans_unf.topLevel q t3 t3'); *)

  run_time 1 "transUnf1" @@
  run q (fun q -> Trans_unf.topLevel q t1 t1')
  ;

  run_time 3 "transUnf2" @@
  run q (fun q -> Trans_unf.topLevel q t2 t2')
  ;

  run_time 3 "transUnf3" @@
  run q (fun q -> Trans_unf.topLevel q t3 t3')


let _ =
  (* run_formula 5 "spec" @@
  run qrs (fun q r s -> Spec.topLevel q r s) *)
  (* run_subst 1 "cpd1" @@
  run q (fun q -> Cpd.topLevel q t1 t1')
  ;

  run_subst 3 "cpd2" @@
  run q (fun q -> Cpd.topLevel q t2 t2')
  ;

  run_subst 3 "cpd3" @@
  run q (fun q -> Cpd.topLevel q t3 t3'); *)

  run_time 1 "cpd1" @@
  run q (fun q -> Cpd.topLevel q t1 t1')
  ;

  run_time 3 "cpd2" @@
  run q (fun q -> Cpd.topLevel q t2 t2')
  ;

  run_time 3 "cpd3" @@
  run q (fun q -> Cpd.topLevel q t3 t3')

let _ =
  (* run_formula 5 "spec" @@
  run qrs (fun q r s -> Spec.topLevel q r s) *)
  run_subst 1 "branches1" @@
  run q (fun q -> Branches.topLevel q t1 t1')
  ;

  run_subst 3 "branches2" @@
  run q (fun q -> Branches.topLevel q t2 t2')
  ;
(*
  run_subst 3 "branches3" @@
  run q (fun q -> Branches.topLevel q t3 t3'); *)

  run_time 1 "branches1" @@
  run q (fun q -> Branches.topLevel q t1 t1')
  ;

  run_time 3 "branches2" @@
  run q (fun q -> Branches.topLevel q t2 t2')
  ;
(*
  run_time 1 "branches3: 1 formula, not 3" @@
  run q (fun q -> Branches.topLevel q t3 t3') *) *)
