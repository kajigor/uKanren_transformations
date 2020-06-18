open OCanren
open GT


let show_list t = show(Std.List.logic) (show(Std.Nat.logic)) @@ t

let reify_list g = g#reify (Std.List.reify Std.Nat.reify)

let list_to_string g = show_list (reify_list g)

let run_list n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun list -> Printf.printf "%s\n" (list_to_string list)) @@
            RStream.take ~n:n @@ r (fun list -> list)

let show_num_pair t = show(Std.Pair.logic) (show(Std.Nat.logic)) (show(Std.Nat.logic)) @@ t

let reify_num_pair g = g#reify (Std.Pair.reify Std.Nat.reify Std.Nat.reify)

let num_pair_to_string g = show_num_pair (reify_num_pair g)

let run_2_nums n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun list -> Printf.printf "%s\n" (num_pair_to_string list)) @@
            RStream.take ~n:n @@ r (fun p -> p)

let run_time n text r =
  let t = Sys.time() in
  let fx = RStream.take ~n:n @@ r (fun fm -> fm) in
  Printf.printf "%s\t%fs\n" text (Sys.time() -. t);
  fx

let take n fn = fun goal -> RStream.take ~n:n @@ (fn goal) (fun fm -> fm)

let run_latency n text goal =
  Benchmark.latency1 15000L ~name:text take goal


let do_tables m n fn lst =
  let samples = Benchmark.latencyN m (List.map (fun (name, goal) -> (name, take n fn, goal)) lst) in
  Benchmark.tabulate samples

