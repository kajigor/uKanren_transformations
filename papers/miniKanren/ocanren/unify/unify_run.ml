open GT
open OCanren
open OCanren.Std
open Tester

open Unify_fun

let show_number x =
  let rec nat2int =
    function
    | Z   -> 0
    | S x -> 1 + nat2int x in
  Printf.sprintf "%d" @@ nat2int x

let show_lnumber x =
  let rec nat2int x =
    match x with
    | Var _       -> 0, Some (show(logic) (fun _ -> "") x)
    | Value Z     -> 0, None
    | Value (S n) -> let a, s = nat2int n in a + 1, s in
  match nat2int x with
  | (n, None)   -> Printf.sprintf "%d" n
  | (0, Some s) -> Printf.sprintf "%s" s
  | (n, Some s) -> Printf.sprintf "(%d + %s)" n s

let show_llist f x =
  let rec show_list x = Printf.sprintf "[%s]" (String.concat "; " x) in
  let rec show_llist x =
    let open OCanren.Std.List in
    match x with
    | Var _               -> [], Some (show(logic) (fun _ -> "") x)
    | Value Nil           -> [], None
    | Value (Cons (x,xs)) -> let l, q = show_llist xs in f x :: l, q in
  match show_llist x with
  | [], None   -> "[]"
  | [], Some s -> s
  | x , None   -> show_list x
  | x , Some s -> Printf.sprintf "%s ^ %s" (show_list x) s


let rec show_gterm f g = function
| Var_ v        -> Printf.sprintf "V %s" (f v)
| Constr (n, a) -> Printf.sprintf "C %s %s" (f n) (g a)

let rec show_term f x = show_gterm f (show(List.ground) (show_term f)) x
let rec show_lterm f x = show(logic) (show_gterm f (show (List.logic) (show_lterm f))) x

let my_show x = show List.ground (show Option.ground (show_term show_number)) x
let my_lshow x = show_llist (show Option.logic (show_lterm show_lnumber)) x
let rec nat_reifier x = For_gnat.reify nat_reifier x
let rec term_reifier r x = For_gterm.reify r (List.reify (term_reifier r)) x
let my_reifier = List.reify (Option.reify (term_reifier nat_reifier))

let full_run x = runR my_reifier my_show my_lshow x

(*******************************************)

let rec int2nat n = if n <= 0 then z () else s (int2nat (n - 1))
let v n           = var_ (int2nat n)
let c n a         = constr (int2nat n) @@ List.list a

let t1  = c 0 [v 0; c 1 []]
let t1' = c 0 [c 1 []; v 0]

let t2, t2' =
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

let t3, t3' =
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


(*******************************************)

let _ =
  full_run (1) q qh ("answers1", (fun q -> check_uni q t1 t1' !!true));
  full_run (1) q qh ("answers2", (fun q -> check_uni q t2 t2' !!true));
  full_run (1) q qh ("answers3", (fun q -> check_uni q t3 t3' !!true));
