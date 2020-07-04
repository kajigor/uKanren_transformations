open GT
open OCanren
open Benchmark

@type ('nat, 'term, 'list) aterm =
  Var_   of 'nat
| Constr of 'nat * 'list
with show, gmap

@type ground_term = (Std.Nat.ground, ground_term, ground_term Std.List.ground) aterm       with show, gmap
@type logic_term  = (Std.Nat.logic , logic_term , logic_term  Std.List.logic ) aterm logic with show, gmap

module ATerm = Fmap3 (struct
                        type ('a, 'b, 'c) t = ('a, 'b, 'c) aterm
                        let fmap f g h = gmap(aterm) f g h
                      end)

let var_ x = inj @@ ATerm.distrib (Var_ x)
let constr x y = inj @@ ATerm.distrib (Constr (x, y))

let rec reify_term t = ATerm.reify Std.Nat.reify reify_term (Std.List.reify reify_term) t

let term_to_string t = (show(logic_term)) (t#reify reify_term)

let subst_to_string subst = (show(Std.List.logic) (show(Std.Option.logic) (show(logic_term)))) (subst#reify (Std.List.reify (Std.Option.reify reify_term)))

let run_formula n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun (subst, t1, t2) -> Printf.printf "%s\t%s\t%s\n" (subst_to_string subst) (term_to_string t1) (term_to_string t2)) @@
            RStream.take ~n:n @@ r (fun subst t1 t2 -> (subst, t1, t2))


let run_subst n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun subst -> Printf.printf "%s\n" (subst_to_string subst)) @@
            RStream.take ~n:n @@ r (fun subst -> subst)

let run_time n text r =
  let t = Sys.time() in
  let fx = RStream.take ~n:n @@ r (fun fm -> fm) in
  Printf.printf "%s: %fs\n" text (Sys.time() -. t);
  fx

let take n fn = fun goal -> RStream.take ~n:n @@ (fn goal) (fun fm -> fm)

let sum_time res =
  List.fold_left (fun x y -> x +. y.wall) 0.0 res

let merge2 fst snd =
  List.map2 (fun (n, x) (m, y) -> if n = m then (n, x @ [y]) else raise @@ Invalid_argument "Different row names") fst snd

let merge_results lst =
  List.fold_left merge2
                 (List.map (fun (n, x) -> (n, [x])) @@ List.hd lst)
                 (List.tl lst)

let res_to_string lst =
  String.concat "," @@
  List.map (fun res -> Printf.sprintf "%f" (sum_time res)) lst

let to_csv file samples_list =
  let oc = open_out file in
  let (names, samples) = List.split samples_list in
  Printf.fprintf oc ",%s\n" (String.concat "," @@ names) ;
  List.iter (fun (name, res) -> Printf.fprintf oc "%s,%s\n" name (res_to_string res)) (merge_results samples);
  close_out oc

let do_tables m n fn lst name =
  let samples = Benchmark.latencyN m (List.map (fun (name, goal) -> (name, take n fn, goal)) lst) in
  Benchmark.tabulate samples;
  (name, samples)

