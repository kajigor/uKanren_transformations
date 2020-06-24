open OCanren
open GT

@type ltype =
| Integer
| Boolean
with show, gmap

@type ('t, 'v, 'bc, 'ic) term =
| BConst of 'bc
| IConst of 'ic
| Var of 'v
| Plus of 't * 't
| Mult of 't * 't
| Eq of 't * 't
| Lt of 't * 't
| Let of 't * 't
| If of 't * 't * 't
with show, gmap

@type ground_term = (ground_term, Std.Nat.ground, Std.Bool.ground, Std.Nat.ground) term with show, gmap
@type logic_term  = (logic_term,  Std.Nat.logic,  Std.Bool.logic,  Std.Nat.logic ) term logic with show, gmap

module Term = Fmap4 (struct
                       type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) term
                       let fmap f g h i = gmap(term) f g h i
                     end)

let bConst_ x = inj @@ Term.distrib (BConst x)
let iConst_ x = inj @@ Term.distrib (IConst x)
let var_ x = inj @@ Term.distrib (Var x)
let plus_ x y = inj @@ Term.distrib (Plus (x, y))
let mult_ x y = inj @@ Term.distrib (Mult (x, y))
let equal_ x y = inj @@ Term.distrib (Eq (x, y))
let less_ x y = inj @@ Term.distrib (Lt (x, y))
let let_ x y = inj @@ Term.distrib (Let (x, y))
let if_ x y z = inj @@ Term.distrib (If (x, y, z))

let integer () = !!Integer
let boolean () = !!Boolean

let rec reify_term t = Term.reify reify_term Std.Nat.reify Std.Bool.reify Std.Nat.reify t
let term_to_string t = (show(logic_term)) (t#reify reify_term)

let run_formula n textRepr r =
  Printf.printf "-----------------------------\n%s\n" textRepr;
  List.iter (fun fm -> Printf.printf "%s\n" (term_to_string fm)) @@ RStream.take ~n:n @@
            r (fun fm -> fm)

let take n fn = fun goal -> RStream.take ~n:n @@ (fn goal) (fun fm -> fm)

let do_tables n fn lst =
  let samples = Benchmark.latencyN 10L (List.map (fun (name, goal) -> (name, take n fn, goal)) lst) in
  Benchmark.tabulate samples
