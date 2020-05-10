open OCanren
open GT

module Helper =
  struct
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
      Printf.printf "-----------------------------\n%s\n" textRepr;
      List.iter (fun (q, r, fm) -> Printf.printf "O:\t%s\tS(O):\t%s\tFm:\t%s\n" (var_to_string q) (var_to_string r) (fm_to_string fm)) @@ RStream.take ~n:n @@
                r (fun q r fm -> (q, r, fm))

    let run_time n text r =
      let t = Sys.time() in
      let fx = RStream.take ~n:n @@ r (fun _ _ fm -> fm) in
      Printf.printf "%s: %fs\n" text (Sys.time() -. t);
      fx
  end

module Original =
  struct
    open Helper

    let rec elemo xs n v =
      fresh (t n1 h) (
        conde [
          n === Std.Nat.zero &&& (xs === Std.( % ) h t) &&& (h === v);
          n === Std.Nat.succ n1 &&& (xs === Std.( % ) h t) &&& (elemo t n1 v)
        ]
      )

    let ando x y r =
      conde [
        (x === !!true ) &&& (y === !!true ) &&& (r === !!true);
        (x === !!true ) &&& (y === !!false) &&& (r === !!false);
        (x === !!false) &&& (y === !!true ) &&& (r === !!false);
        (x === !!false) &&& (y === !!false) &&& (r === !!false)
      ]

    let oro x y r =
      conde [
        (x === !!true ) &&& (y === !!true ) &&& (r === !!true);
        (x === !!true ) &&& (y === !!false) &&& (r === !!true);
        (x === !!false) &&& (y === !!true ) &&& (r === !!true);
        (x === !!false) &&& (y === !!false) &&& (r === !!false)
      ]

    let noto x r =
      conde [
        (x === !!true)  &&& (r === !!false);
        (x === !!false) &&& (r === !!true)
      ]


    let rec evalo st fm u =
      fresh (x y z v w) (
        conde [
          fm === conj x y
             &&& ando v w u
             &&& evalo st x v
             &&& evalo st y w;
          fm === disj x y
             &&& oro  v w u
             &&& evalo st x v
             &&& evalo st y w;
          fm === neg  x
             &&& noto v u
             &&& evalo st x v;
          fm === var  z
             &&& elemo st z u
      ])

    let topLevel st fm = evalo st fm (!!true)

    let _ =
        (* run_formula 10 "original" @@
        run qrs (fun r t fm ->
          topLevel (Std.(%<) r t) fm); *)

      run_time 1000 "original" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm)
  end


module Ecce =
  struct
    open Helper

    let topLevel y1 y2 =
      let rec eval z1 z2 z3 = fresh (fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === !!true) &&& eval__1 fA fB)
      and eval__1 z1 z2 =
        fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!true)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& eval__1 fA fB &&& eval__1 fA fC)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& or_conj__3 fA fB fC)
        ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__4 fA fB)
      and elem__2 z1 z2 z3 z4 =
        fresh (fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fA))
        ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem__2 fA fC fD fE)
      and or_conj__3 z1 z2 z3 =
        fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__1 fA fC)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__4 fA fC)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__1 fA fC)
      and eval__4 z1 z2 =
        fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!false)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& and_conj__5 fA fB fC)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& eval__4 fA fB &&& eval__4 fA fC)
        ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__1 fA fB)
      and and_conj__5 z1 z2 z3 =
        fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__1 fA fB &&& eval__4 fA fC)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__1 fA fC)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === fC) &&& eval__4 fA fB &&& eval__4 fA fC)
      in
      eval__1 y1 y2

    let _ =
      (* run_formula 30 "ecce" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm); *)

      run_time 1000 "ecce" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm)
  end


module Transformed =
  struct
    open Helper

    let topLevel x0 x1 =
      let rec _evalo y2 y3 =
        fresh (q1 q2 q3 q4 q5 q6)
          ( y2 === conj q1 q2 &&& evaloEvalo y3 q1 q2
          ||| (y2 === disj q1 q2 &&& (evaloEvalo y3 q1 q2 ||| (__evalo y3 q1 &&& _evalo q2 y3) ||| ___evaloEvalo y3 q1 q2))
          ||| (y2 === neg q1 &&& __evalo y3 q1)
          ||| (y2 === var q3 &&& (q3 === Std.Nat.zero &&& (y3 === Std.( % ) !!true q4) ||| (q3 === Std.Nat.succ q5 &&& (y3 === Std.( % ) q6 q4) &&& _elemo q4 q5)))
          )
      and evaloEvalo y4 y5 y6 = _evalo y5 y4 &&& _evalo y6 y4
      and _evaloEvalo y7 y8 y9 = __evalo y7 y8 &&& _evalo y9 y7
      and __evalo y10 y11 =
        fresh (q1 q2 q3 q4 q5 q6)
          ( y11 === conj q1 q2
          &&& (_evaloEvalo y10 q1 q2 ||| (_evalo q1 y10 &&& __evalo y10 q2) ||| __evaloEvalo y10 q1 q2)
          ||| (y11 === disj q1 q2 &&& __evaloEvalo y10 q1 q2)
          ||| (y11 === neg q1 &&& _evalo q1 y10)
          ||| ( y11 === var q3 &&& (q3 === Std.Nat.zero &&& (y10 === Std.( % ) !!false q4) ||| (q3 === Std.Nat.succ q5 &&& (y10 === Std.( % ) q6 q4) &&& elemo q4 q5)) ) )
      and __evaloEvalo y12 y13 y14 = __evalo y12 y13 &&& __evalo y12 y14
      and elemo y15 y16 =
        fresh (q1 q2 q3) (y16 === Std.Nat.zero &&& (y15 === Std.( % ) !!false q1) ||| (y16 === Std.Nat.succ q2 &&& (y15 === Std.( % ) q3 q1) &&& elemo q1 q2))
      and ___evaloEvalo y17 y18 y19 = _evalo y18 y17 &&& __evalo y17 y19
      and _elemo y20 y21 =
        fresh (q1 q2 q3) (y21 === Std.Nat.zero &&& (y20 === Std.( % ) !!true q1) ||| (y21 === Std.Nat.succ q2 &&& (y20 === Std.( % ) q3 q1) &&& _elemo q1 q2))
      in
      _evalo x1 x0

    let _ =
(*
      run_formula 30 "transformed" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm); *)

      run_time 1000 "transformed" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm)
  end
