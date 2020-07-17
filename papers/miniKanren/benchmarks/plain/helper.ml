open OCanren
open GT

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

let rec reify_f f = F.reify reify_f Std.Nat.reify f

let var_to_string r = (show(logic) (show(bool))) (r#reify reify)
let fm_to_string fm = (show(f)) (fm#reify reify_f)

let run_formula n textRepr r =
  let conj_before = Peep.conj_counter () in
  let disj_before = Peep.disj_counter () in
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun (q, r, fm) -> Printf.printf "O:\t%s\tS(O):\t%s\tFm:\t%s\n" (var_to_string q) (var_to_string r) (fm_to_string fm)) @@ RStream.take ~n:n @@
            r (fun q r fm -> (q, r, fm));
  let conj_after = Peep.conj_counter () in
  let disj_after = Peep.disj_counter () in
  Printf.printf "\nDisj: %s\nConj: %s\n" (string_of_int (disj_after - disj_before)) (string_of_int (conj_after - conj_before))


let run_time n text r =
  let t = Sys.time() in
  let fx = RStream.take ~n:n @@ r (fun _ _ fm -> fm) in
  Printf.printf "%s: %fs\n" text (Sys.time() -. t);
  fx

let take n fn = fun goal -> RStream.take ~n:n @@ (fn goal) (fun fm -> fm)

let do_tables m n fn lst =
  let samples = Benchmark.latencyN m (List.map (fun (name, goal) -> (name, take n fn, goal)) lst) in
  Benchmark.tabulate samples
