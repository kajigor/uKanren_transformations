open GT
open OCanren

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

let do_tables iters n fn lst =
  let samples = Benchmark.latencyN iters (List.map (fun (name, goal) -> (name, take n fn, goal)) lst) in
  Benchmark.tabulate samples


(* let rec eq_nat a b q36 =
    (conde
        [
          (a === (Std.Nat.zero)) &&& (b === Std.Nat.zero) &&& (q36 === (!! true));
          fresh (q39) ((a === (Std.Nat.succ q39)) &&& (b === Std.Nat.zero) &&& (q36 === (!! false)));
          fresh (q41) ((a === (Std.Nat.zero)) &&& (b === (Std.Nat.succ q41)) &&& (q36 === (!! false)));
          fresh (x y) ((a === (Std.Nat.succ x)) &&& (b === (Std.Nat.succ y)) &&& (eq_nat x y q36))
        ])

let rec get_term var subst q32 =
  ((subst === (Std.nil ())) &&& (q32 === (Std.none ()))) |||
  (fresh (x xs) (subst === (Std.(%) x xs)) (((var === (Std.Nat.zero)) &&& (x === q32)) |||
                                           (fresh (n) (var === (Std.Nat.succ n)) (get_term n xs q32))))

let rec check_uni subst (t1 : (ground_term, logic_term) Logic.injected) (t2 : (ground_term, logic_term) Logic.injected) q31 =
  let rec forall2 subst l1 l2 q0 =
      (((l1 === (Std.nil ())) &&& (l2 === (Std.nil ())) &&& (q0 === (!! true))) |||
         (fresh (x xs y ys q3 q4) ((l1 === (Std.(%) x xs)) &&& (l2 === (Std.(%) y ys))) (
            check_uni subst x y q3) (forall2 subst xs ys q4) (conde [(q3 === (!! false)) &&& (q0 === (!! false)); (q3 === (!! true)) &&& (q0 === q4)]))) in
    (conde
       [fresh (n1 a1 n2 a2 q12 q13) (
         (t1 === (constr n1 a1)) &&&
         (t2 === (constr n2 a2)) &&&
         (eq_nat n1 n2 q12) &&&
         (forall2 subst a1 a2 q13) &&&
         (conde [(q12 === (!! false)) &&& (q31 === (!! false)); (q12 === (!! true)) &&& (q31 === q13)]));
       fresh (v n a q19) (t1 === (var_ v) &&& t2 === (constr n a)) (get_term v subst q19)
         (((q19 === (Std.none ())) &&& (q31 === (!! false))) ||| (fresh (t) (q19 === (Std.some t)) (check_uni subst t t2 q31)));
       fresh (n a v q22) (t1 === (constr n a) &&& t2 === (var_ v)) (get_term v subst q22)
         (((q22 === (Std.none ())) &&& (q31 === (!! false))) ||| (fresh (t) (q22 === (Std.some t)) (check_uni subst t1 t q31)));
       fresh (v1 v2 q25) (t1 === (var_ v1) &&& t2 === (var_ v2)) (get_term v1 subst q25)
         ((fresh (t1') (q25 === (Std.some t1')) (check_uni subst t1' t2 q31)) |||
            (fresh (q27) (q25 === (Std.none ())) (get_term v2 subst q27) ((fresh (q28) (q27 === (Std.some q28)) (q31 === (!! false))) ||| ((q27 === (Std.none ())) &&& (eq_nat v1 v2 q31)))))]) *)
