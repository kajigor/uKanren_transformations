open OCanren
open GT

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