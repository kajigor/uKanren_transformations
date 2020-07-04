open OCanren
open GT
open Benchmark

let show_graph t = show(Std.List.logic) ((show(Std.Pair.logic) (show(Std.Nat.logic)) (show(Std.Nat.logic)))) @@ t

let reify_graph g = g#reify (Std.List.reify (Std.Pair.reify Std.Nat.reify Std.Nat.reify))

let graph_to_string g = show_graph (reify_graph g)

let run_graph n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun graph -> Printf.printf "%s\n" (graph_to_string graph)) @@
            RStream.take ~n:n @@ r (fun graph -> graph)

let show_path t = show(Std.List.logic) (show(Std.Nat.logic)) @@ t

let reify_path g = g#reify (Std.List.reify Std.Nat.reify)

let path_to_string g = show_path (reify_path g)

let run_path n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun path -> Printf.printf "%s\n" (path_to_string path)) @@
            RStream.take ~n:n @@ r (fun path -> path)


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
