open OCanren
open GT
open Benchmark

@type ('a, 'b) fa =
| Conj of 'a * 'a
| Disj of 'a * 'a
| Neg  of 'a
| Var  of 'b
with show, gmap

@type f = (f, Std.Nat.logic) fa logic with show, gmap

module F = Fmap2 (struct type ('a, 'b) t = ('a, 'b) fa let fmap f g x = gmap(fa) f g x end)

let conj x y = inj @@ F.distrib (Conj (x, y))
let disj x y = inj @@ F.distrib (Disj (x, y))
let var  x   = inj @@ F.distrib (Var x)
let neg  x   = inj @@ F.distrib (Neg x)

let max x y m =
  ((Std.Nat.geo x y (!!true)) &&& (m === x)) ||| ((Std.Nat.leo x y (!!true)) &&& (m === y))


let rec depth fm d =
  (fresh (z) ((fm === var z) &&& (d === Std.Nat.zero))) |||
  (fresh (x dx) ((fm === neg x) &&& (d === Std.Nat.succ dx) &&& (depth x dx))) |||
  (fresh (x y dx dy m) ((fm === conj x y) &&& (depth x dx) &&& (depth y dy) &&& (max dx dy m) &&& (d === Std.Nat.succ m))) |||
  (fresh (x y dx dy m) ((fm === disj x y) &&& (depth x dx) &&& (depth y dy) &&& (max dx dy m) &&& (d === Std.Nat.succ m)))


let rec reify_f f = F.reify reify_f Std.Nat.reify f

let var_to_string r = (show(logic) (show(bool))) (r#reify reify)
let fm_to_string fm = (show(f)) (fm#reify reify_f)

let run_formula n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun (q, r, fm) -> Printf.printf "O:\t%s\tS(O):\t%s\tFm:\t%s\n" (var_to_string q) (var_to_string r) (fm_to_string fm)) @@ RStream.take ~n:n @@
            r (fun q r fm -> (q, r, fm))

let run_time n text r =
  let t = Sys.time() in
  let fx = RStream.take ~n:n @@ r (fun _ _ fm -> fm) in
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
