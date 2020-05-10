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

    let show_env c = show(Std.List.logic) (show(logic) (show(bool))) c
    let reify_env = Std.List.reify reify

    let run_st textRepr r =
      Printf.printf "\n-----------------------------\n%s\n" textRepr;
      List.iter (fun s ->
          Printf.printf "%s\n" @@ show_env
                                  (s#reify reify_env)) @@ RStream.take ~n:10 @@
          r (fun st -> st)

    let run_st1 textRepr r =
      Printf.printf "\n-----------------------------\n%s\n" textRepr;
      List.iter (fun s ->
          Printf.printf "%s\n" @@ show_env
                                  (s#reify reify_env)) @@ RStream.take ~n:3 @@
          r (fun st r -> st)

    let run_st2 textRepr r =
      Printf.printf "\n-----------------------------\n%s\n" textRepr;
      List.iter (fun s ->
          Printf.printf "%s\n" @@ show_env
                                  (s#reify reify_env)) @@ RStream.take ~n:3 @@
          r (fun st r t -> st)

    let run_formula n textRepr r =
      Printf.printf "-----------------------------\n%s\n" textRepr;
      List.iter (fun (q, r, fm) -> Printf.printf "O:\t%s\tS(O):\t%s\tFm:\t%s\n" (var_to_string q) (var_to_string r) (fm_to_string fm)) @@ RStream.take ~n:n @@
                r (fun q r fm -> (q, r, fm))
  end
(*
module Ecce =
  struct
    open Helper

    (* let rec eval__1 st fm r =
      conde [
        fresh (a b c) (
              st === Std.List.(%) a b
          &&& fm === var c
          &&& elem__2 c a b (!!true)
        );
        fresh (a b c) (
              st === a
          &&& fm === conj b c
          &&& eval__1 a b
          &&& eval__1 a c
        );
        fresh (a b c) (
              st === a
          &&& fm === disj b c
          &&& eval_conj__3 a b c
        );
        fresh (a b c d e f g h i ) (
              st === a
          &&& fm === not b
          &&& eval__4 a b
        )
      ]
    and elem__2 n x xs r =
      conde [
        fresh (a b) (n === Std.Nat.zero &&& x === a &&& xs === b &&& r === a);
        fresh (a b c d e) (
              n === Std.Nat.succ a
          &&& x === b
          &&& xs === Std.List.(%) c d
          &&& r === e
          &&& elem__2 a c d e
        )
      ]
    and eval_conj__3 xs fm r =
      conde [
        fresh (a b c d e f g) (
          xs === Std.List.(%) a b
          &&& fm === var c
          &&& r === d
          &&& elem__2 c a b e
          &&& eval__13 a b d f
          &&& nand__8 e g
          &&& nand_conj__14 f g
        );
        fresh (a b c d e f g h i) (
          xs === a
          &&& fm === conj b c
          &&& r === d
          &&& eval__6 a b e
          &&& eval__6 a c f
          &&& nand_conj__12 e f g
          &&& eval__6 a d h
          &&& nand__8 g i
          &&& nand_conj__14 h i
        );
        fresh (a b c d e f g h i j) (
          xs === a
          &&& fm === disj b c
          &&& r === d
          &&& eval__6 a b e
          &&& eval__6 a c f
          &&& nand__8 e g
          &&& nand_conj__9 f g h
          &&& eval__6 a d i
          &&& nand__8 h j
          &&& nand_conj__14 i j
        );
        fresh (a b c d e) (
          xs === a
          &&& fm === not b
          &&& r === c
          &&& eval_conj__15 a b c d e
          &&& nand__16 d e
        )
      ]
    and eval__4 st fm =
      conde [
        fresh (a b c) (
          st === % a b
          &&& fm === var c
          &&& elem__2 c a b false
        );
        fresh (a b c d) (
          st = a
          & fm = conj b c
          & eval_conj__5 a b d c d
        );
        fresh () (
          st = a
          & fm = disj b c
          & eval__4 a b
          & eval__4 a c
        );
        fresh ( ) (
          st = a
          & fm = not b
          & eval__1 a b
        )
      ]
    and eval_conj__5 st fm x y z =
      conde [
        fresh () (
          st = % a b
          & fm = var c
          & x = d
          & y = e
          & z = f
          & elem__2 c a b d
          & eval__13 a b e g
          & nand__7 f g (!!true)
        );
        fresh () (
          st = a
          fm = conj b c
          x = d
          y = e
          z = f
        );
        fresh () ();
        fresh () ();
      ]


  eval_conj__5(a conj(b c) d e f) :-
    eval__6(a b g)
    eval__6(a c h)
    nand_conj__12(g h d)
    eval__6(a e i)
    nand__7(f i (!!true))
  eval_conj__5(a disj(b c) d e f) :-
    eval__6(a b g)
    eval__6(a c h)
    nand__8(g i)
    nand_conj__9(h i d)
    eval__6(a e j)
    nand__7(f j (!!true))
  eval_conj__5(a not(b) c d e) :-
    eval__6(a b f)
    nand__8(c f)
    eval__6(a d g)
    nand__7(e g (!!true))


    let topLevel st fm = eval__1 st fm *)


    let topLevel y1 y2 =
      let rec eval z1 z2 z3 = fresh (fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === !!true) &&& eval__1 fA fB)
      and eval__1 z1 z2 =
        fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!true)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& eval__1 fA fB &&& eval__1 fA fC)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& eval_conj__3 fA fB fC)
        ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__4 fA fB)
      and elem__2 z1 z2 z3 z4 =
        fresh (fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fA))
        ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem__2 fA fC fD fE)
      and eval_conj__3 z1 z2 z3 =
        fresh (fG fF fE fD fC fB fA)
          (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& elem__2 fC fA fB fE &&& eval__13 fA fB fD fF &&& nand__8 fE fG &&& nand_conj__14 fF fG)
        ||| fresh (fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand_conj__12 fE fF fG &&& eval__6 fA fD fH &&& nand__8 fG fI &&& nand_conj__14 fH fI
              )
        ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand__8 fE fG &&& nand_conj__9 fF fG fH &&& eval__6 fA fD fI &&& nand__8 fH fJ
              &&& nand_conj__14 fI fJ )
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& eval_conj__15 fA fB fC fD fE &&& nand__16 fD fE)
      and eval__4 z1 z2 =
        fresh (fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& elem__2 fC fA fB !!false)
        ||| fresh (fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& eval_conj__5 fA fB fD fC fD)
        ||| fresh (fC fB fA) (z1 === fA &&& (z2 === disj fB fC) &&& eval__4 fA fB &&& eval__4 fA fC)
        ||| fresh (fB fA) (z1 === fA &&& (z2 === neg fB) &&& eval__1 fA fB)
      and eval_conj__5 z1 z2 z3 z4 z5 =
        fresh (fG fF fE fD fC fB fA)
          ( z1 === Std.( % ) fA fB
          &&& (z2 === var fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem__2 fC fA fB fD &&& eval__13 fA fB fE fG &&& nand__7 fF fG !!true )
        ||| fresh (fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand_conj__12 fG fH fD &&& eval__6 fA fE fI
              &&& nand__7 fF fI !!true )
        ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fD
              &&& eval__6 fA fE fJ &&& nand__7 fF fJ !!true )
        ||| fresh (fG fF fE fD fC fB fA)
              ( z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval__6 fA fB fF &&& nand__8 fC fF &&& eval__6 fA fD fG
              &&& nand__7 fE fG !!true )
      and eval__6 z1 z2 z3 =
        fresh (fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& elem__2 fC fA fB fD)
        ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand_conj__12 fE fF fD)
        ||| fresh (fG fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === disj fB fC) &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand__8 fE fG &&& nand_conj__9 fF fG fD)
        ||| fresh (fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& eval__6 fA fB fD &&& nand__8 fC fD)
      and nand__7 z1 z2 z3 =
        z1 === !!false &&& (z2 === !!false) &&& (z3 === !!true)
        ||| (z1 === !!false &&& (z2 === !!true) &&& (z3 === !!true))
        ||| (z1 === !!true &&& (z2 === !!false) &&& (z3 === !!true))
        ||| (z1 === !!true &&& (z2 === !!true) &&& (z3 === !!false))
      and nand__8 z1 z2 = z1 === !!false &&& (z2 === !!true) ||| (z1 === !!true &&& (z2 === !!false))
      and nand_conj__9 z1 z2 z3 =
        fresh (fB fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === fB) &&& nand__10 fA fB)
        ||| fresh (fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === !!true) &&& nand__11 fA)
      and nand__10 z1 z2 = z1 === !!false &&& (z2 === !!true) ||| (z1 === !!true &&& (z2 === !!false))
      and nand__11 z1 = z1 === !!false ||| (z1 === !!true)
      and nand_conj__12 z1 z2 z3 =
        z1 === !!false &&& (z2 === !!false) &&& (z3 === !!false)
        ||| (z1 === !!false &&& (z2 === !!true) &&& (z3 === !!false))
        ||| (z1 === !!true &&& (z2 === !!false) &&& (z3 === !!false))
        ||| (z1 === !!true &&& (z2 === !!true) &&& (z3 === !!true))
      and eval__13 z1 z2 z3 z4 =
        fresh (fD fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === var fC) &&& (z4 === fD) &&& elem__2 fC fA fB fD)
        ||| fresh (fG fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === fB) &&& (z3 === conj fC fD) &&& (z4 === fE) &&& eval__13 fA fB fC fF &&& eval__13 fA fB fD fG &&& nand_conj__12 fF fG fE)
        ||| fresh (fH fG fF fE fD fC fB fA)
              ( z1 === fA &&& (z2 === fB)
              &&& (z3 === disj fC fD)
              &&& (z4 === fE) &&& eval__13 fA fB fC fF &&& eval__13 fA fB fD fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fE )
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === fB) &&& (z3 === neg fC) &&& (z4 === fD) &&& eval__13 fA fB fC fE &&& nand__8 fD fE)
      and nand_conj__14 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| fresh (fA) (z1 === !!true &&& (z2 === fA) &&& nand__11 fA)
      and eval_conj__15 z1 z2 z3 z4 z5 =
        fresh (fH fG fF fE fD fC fB fA)
          ( z1 === Std.( % ) fA fB
          &&& (z2 === var fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__28 fC fA fB fG &&& eval__13 fA fB fD fH &&& nand__8 fG fE &&& nand__8 fH fF )
        ||| fresh (fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__21 fA fB fC fG &&& eval__6 fA fD fH &&& nand__8 fG fE &&& nand__8 fH fF )
        ||| fresh (fL fK fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
              &&& nand__8 fK fJ &&& eval__6 fA fD fL &&& nand__8 fK fE &&& nand__8 fL fF )
        ||| fresh (fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__17 fA fB fC fF fD &&& nand__8 fF fE)
      and nand__16 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| (z1 === !!false &&& (z2 === !!true)) ||| (z1 === !!true &&& (z2 === !!false))
      and eval_conj__17 z1 z2 z3 z4 z5 =
        fresh (fG fF fE fD fC fB fA)
          ( z1 === Std.( % ) fA fB
          &&& (z2 === var fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__27 fC fA fB fG &&& eval__13 fA fB fD fE &&& nand__8 fG fF )
        ||| fresh (fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__21 fA fB fC fG &&& nand__8 fH fG &&& eval__6 fA fD fE &&& nand__8 fH fF )
        ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
              &&& nand_conj__22 fJ fK &&& eval__6 fA fD fE &&& nand__8 fK fF )
        ||| fresh (fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__18 fA fB fF fC fD &&& nand__8 fF fE)
      and eval_conj__18 z1 z2 z3 z4 z5 =
        fresh (fF fE fD fC fB fA)
          (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__20 fC fA fB fD &&& eval__13 fA fB fE fF)
        ||| fresh (fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__21 fA fB fC fG &&& nand_conj__22 fG fD &&& eval__6 fA fE fF )
        ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
              &&& nand_conj__23 fJ fD &&& eval__6 fA fE fF )
        ||| fresh (fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__19 fA fB fF &&& nand__8 fC fF &&& eval__6 fA fD fE)
      and eval_conj__19 z1 z2 z3 =
        fresh (fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& elem_conj__20 fC fA fB fD)
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& eval_conj__21 fA fB fC fE &&& nand_conj__22 fE fD)
        ||| fresh (fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& eval__6 fA fB fE &&& eval__6 fA fC fF &&& nand__8 fE fG &&& nand_conj__9 fF fG fH &&& nand_conj__23 fH fD )
        ||| fresh (fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& eval_conj__19 fA fB fD &&& nand__8 fC fD)
      and elem_conj__20 z1 z2 z3 z4 =
        fresh (fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& nand_conj__23 fA fC)
        ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem_conj__20 fA fC fD fE)
      and eval_conj__21 z1 z2 z3 z4 =
        fresh (fG fF fE fD fC fB fA)
          (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem__2 fC fA fB fF &&& eval__13 fA fB fD fG &&& nand_conj__32 fF fG fE)
        ||| fresh (fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand_conj__12 fF fG fH &&& eval_conj__24 fA fD fH fE )
        ||| fresh (fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& eval_conj__24 fA fD fI fE
              )
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__25 fA fB fC fE &&& nand__8 fD fE)
      and nand_conj__22 z1 z2 = z1 === !!true &&& (z2 === !!true) ||| (z1 === !!false &&& (z2 === !!false))
      and nand_conj__23 z1 z2 = z1 === !!true &&& (z2 === !!false) ||| (z1 === !!false &&& (z2 === !!true))
      and eval_conj__24 z1 z2 z3 z4 =
        fresh (fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__29 fC fA fB fD fE)
        ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__30 fA fB fC fD fF &&& nand_conj__31 fF fE)
        ||| fresh (fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__32 fD fI fE )
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__33 fA fB fC fE &&& nand__8 fD fE)
      and eval_conj__25 z1 z2 z3 z4 =
        fresh (fG fF fE fD fC fB fA)
          ( z1 === Std.( % ) fA fB
          &&& (z2 === var fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__28 fC fA fB fF &&& eval__13 fA fB fD fG &&& nand_conj__12 fF fG fE )
        ||| fresh (fG fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& eval__6 fA fD fG &&& nand_conj__12 fF fG fE)
        ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand__8 fJ fI
              &&& eval__6 fA fD fK &&& nand_conj__12 fJ fK fE )
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__26 fA fB fC fE &&& nand__8 fE fD)
      and eval_conj__26 z1 z2 z3 z4 =
        fresh (fG fF fE fD fC fB fA)
          (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__27 fC fA fB fF &&& eval__13 fA fB fD fG &&& nand__7 fF fG fE)
        ||| fresh (fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& nand__8 fG fF &&& eval__6 fA fD fH &&& nand__7 fG fH fE )
        ||| fresh (fK fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__22 fI fJ
              &&& eval__6 fA fD fK &&& nand__7 fJ fK fE )
        ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__18 fA fB fE fC fF &&& nand__7 fE fF fD)
      and elem_conj__27 z1 z2 z3 z4 =
        fresh (fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& nand_conj__22 fA fC)
        ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem_conj__27 fA fC fD fE)
      and elem_conj__28 z1 z2 z3 z4 =
        fresh (fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& nand__8 fC fA)
        ||| fresh (fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& elem_conj__28 fA fC fD fE)
      and elem_conj__29 z1 z2 z3 z4 z5 =
        fresh (fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& nand_conj__32 fC fA fD)
        ||| fresh (fF fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__29 fA fC fD fE fF)
      and eval_conj__30 z1 z2 z3 z4 z5 =
        fresh (fJ fI fH fG fF fE fD fC fB fA)
          ( z1 === Std.( % ) fA fB
          &&& (z2 === var fC)
          &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem__2 fC fA fB fG &&& eval__13 fA fB fD fH &&& nand__7 fG fH fI &&& nand__8 fI fJ
          &&& nand__7 fE fJ fF )
        ||| fresh (fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === conj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand_conj__12 fG fH fI &&& eval_conj__41 fA fD fI fE fF
              )
        ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
              &&& eval_conj__41 fA fD fJ fE fF )
        ||| fresh (fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__25 fA fB fC fF &&& nand__7 fD fF fE)
      and nand_conj__31 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| (z1 === !!true &&& (z2 === !!true))
      and nand_conj__32 z1 z2 z3 =
        z1 === !!false &&& (z2 === !!false) &&& (z3 === !!true)
        ||| (z1 === !!false &&& (z2 === !!true) &&& (z3 === !!true))
        ||| (z1 === !!true &&& (z2 === !!false) &&& (z3 === !!true))
        ||| (z1 === !!true &&& (z2 === !!true) &&& (z3 === !!false))
      and eval_conj__33 z1 z2 z3 z4 =
        fresh (fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__34 fC fA fB fD fE)
        ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& nand_conj__12 fD fF fE)
        ||| fresh (fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__35 fI fD fE )
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__36 fA fB fC fE &&& nand__8 fE fD)
      and elem_conj__34 z1 z2 z3 z4 z5 =
        fresh (fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& nand_conj__35 fA fC fD)
        ||| fresh (fF fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__34 fA fC fD fE fF)
      and nand_conj__35 z1 z2 z3 =
        fresh (fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === !!false) &&& nand__11 fA)
        ||| fresh (fB fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === fB) &&& nand_conj__40 fA fB)
      and eval_conj__36 z1 z2 z3 z4 =
        fresh (fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& elem_conj__37 fC fA fB fD fE)
        ||| fresh (fF fE fD fC fB fA) (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& eval_conj__21 fA fB fC fF &&& nand_conj__38 fF fD fE)
        ||| fresh (fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& eval__6 fA fB fF &&& eval__6 fA fC fG &&& nand__8 fF fH &&& nand_conj__9 fG fH fI &&& nand_conj__39 fI fD fE )
        ||| fresh (fE fD fC fB fA) (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& eval_conj__19 fA fB fE &&& nand__7 fC fE fD)
      and elem_conj__37 z1 z2 z3 z4 z5 =
        fresh (fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& nand_conj__39 fA fC fD)
        ||| fresh (fF fE fD fC fB fA) (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__37 fA fC fD fE fF)
      and nand_conj__38 z1 z2 z3 =
        fresh (fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === !!true) &&& nand__11 fA)
        ||| fresh (fB fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === fB) &&& nand__10 fA fB)
      and nand_conj__39 z1 z2 z3 =
        fresh (fB fA) (z1 === !!true &&& (z2 === fA) &&& (z3 === fB) &&& nand__10 fA fB)
        ||| fresh (fA) (z1 === !!false &&& (z2 === fA) &&& (z3 === !!true) &&& nand__11 fA)
      and nand_conj__40 z1 z2 = z1 === !!false &&& (z2 === !!false) ||| (z1 === !!true &&& (z2 === !!true))
      and eval_conj__41 z1 z2 z3 z4 z5 =
        fresh (fF fE fD fC fB fA) (z1 === Std.( % ) fA fB &&& (z2 === var fC) &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& elem_conj__42 fC fA fB fD fE fF)
        ||| fresh (fG fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === conj fB fC) &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval_conj__30 fA fB fC fD fG &&& nand_conj__9 fG fE fF)
        ||| fresh (fJ fI fH fG fF fE fD fC fB fA)
              ( z1 === fA
              &&& (z2 === disj fB fC)
              &&& (z3 === fD) &&& (z4 === fE) &&& (z5 === fF) &&& eval__6 fA fB fG &&& eval__6 fA fC fH &&& nand__8 fG fI &&& nand_conj__9 fH fI fJ
              &&& nand_conj__43 fD fJ fE fF )
        ||| fresh (fF fE fD fC fB fA)
              (z1 === fA &&& (z2 === neg fB) &&& (z3 === fC) &&& (z4 === fD) &&& (z5 === fE) &&& eval_conj__33 fA fB fC fF &&& nand__7 fD fF fE)
      and elem_conj__42 z1 z2 z3 z4 z5 z6 =
        fresh (fE fD fC fB fA) (z1 === Std.Nat.zero &&& (z2 === fA) &&& (z3 === fB) &&& (z4 === fC) &&& (z5 === fD) &&& (z6 === fE) &&& nand_conj__43 fC fA fD fE)
        ||| fresh (fG fF fE fD fC fB fA)
              (z1 === Std.Nat.succ fA &&& (z2 === fB) &&& (z3 === Std.( % ) fC fD) &&& (z4 === fE) &&& (z5 === fF) &&& (z6 === fG) &&& elem_conj__42 fA fC fD fE fF fG)
      and nand_conj__43 z1 z2 z3 z4 =
        fresh (fA) (z1 === !!false &&& (z2 === !!false) &&& (z3 === fA) &&& (z4 === !!true) &&& nand__11 fA)
        ||| fresh (fA) (z1 === !!false &&& (z2 === !!true) &&& (z3 === fA) &&& (z4 === !!true) &&& nand__11 fA)
        ||| fresh (fA) (z1 === !!true &&& (z2 === !!false) &&& (z3 === fA) &&& (z4 === !!true) &&& nand__11 fA)
        ||| fresh (fB fA) (z1 === !!true &&& (z2 === !!true) &&& (z3 === fA) &&& (z4 === fB) &&& nand__10 fA fB)
      in
      eval__1 y1 y2

    let _ =
      let x = Std.nat 0 in
      let y = Std.nat 1 in

      run_st "not x && x" @@
      run q (fun st ->
        topLevel st (conj (neg @@ var x) (var x))) ;

      run_st "not x && y" @@
      run q (fun st ->
        topLevel st (conj (neg @@ var x) (var y)));

      run_st "not x || x" @@
      run q (fun st ->
        topLevel st (disj (neg @@ var x) (var x)));


    (*
      (* won't terminate *)
      run_fm1 "\\forall x . not x && x" @@
      run qr (fun st r ->
        topLevel st (conj (neg @@ var r) (var r))) ;
    *)

      run_st2 "\\forall x y . not x && y" @@
      run qrs (fun st r t ->
        topLevel st (conj (neg @@ var r) (var t))) ;

      run_st1 "\\forall x . not x || x" @@
      run qr (fun st r ->
        topLevel st (disj (neg @@ var r) (var r))) ;

      run_formula 10000 "[x, y]" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm)
  end *)

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

    let rec evalo st fm u =
      fresh (x y z v w) (
        conde [
          fm === conj x y &&& evalo st x v
             &&& evalo st y w &&& Std.Bool.ando v w u;
          fm === disj x y &&& evalo st x v
             &&& evalo st y w &&& Std.Bool.oro  v w u;
          fm === neg  x   &&& evalo st x v
             &&& Std.Bool.noto v u ;
          fm === var  z   &&& elemo st z u
      ])

    let topLevel st fm = evalo st fm (!!true)

    let _ =
      let x = Std.nat 0 in
      let y = Std.nat 1 in

      run_st "not x && x" @@
      run q (fun st ->
        topLevel st (conj (neg @@ var x) (var x))) ;

      run_st "not x && y" @@
      run q (fun st ->
        topLevel st (conj (neg @@ var x) (var y)));

      run_st "not x || x" @@
      run q (fun st ->
        topLevel st (disj (neg @@ var x) (var x)));


    (*
      (* won't terminate *)
      run_fm1 "\\forall x . not x && x" @@
      run qr (fun st r ->
        topLevel st (conj (neg @@ var r) (var r))) ;
    *)

      run_st2 "\\forall x y . not x && y" @@
      run qrs (fun st r t ->
        topLevel st (conj (neg @@ var r) (var t))) ;

      run_st1 "\\forall x . not x || x" @@
      run qr (fun st r ->
        topLevel st (disj (neg @@ var r) (var r))) ;

      run_formula 10000 "[x, y]" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm)
  end


(* module Transformed =
  struct
    open Helper

    let topLevel x0 x1 =
      let rec _evalo y2 y3 =
        fresh (q1 q2 q3 q4 q5 q6)
          ( y3 === conj q1 q2 &&& ___evaloEvalo y2 q1 q2
          ||| (y3 === disj q1 q2 &&& (__evalo y2 q1 &&& _evalo y2 q2 ||| __evaloEvalo y2 q1 q2 ||| ___evaloEvalo y2 q1 q2))
          ||| (y3 === neg q1 &&& __evalo y2 q1)
          ||| (y3 === var q3 &&& (q3 === Std.Nat.zero &&& (y2 === Std.( % ) !!true q4) ||| (q3 === Std.Nat.succ q5 &&& (y2 === Std.( % ) q6 q4) &&& _elemo q4 q5)))
          )
      and __evalo y4 y5 =
        fresh (q1 q2 q3 q4 q5 q6)
          ( y5 === conj q1 q2
          &&& (_evaloEvalo y4 q1 q2 ||| evaloEvalo y4 q1 q2 ||| (_evalo y4 q1 &&& __evalo y4 q2))
          ||| (y5 === disj q1 q2 &&& _evaloEvalo y4 q1 q2)
          ||| (y5 === neg q1 &&& _evalo y4 q1)
          ||| (y5 === var q3 &&& (q3 === Std.Nat.zero &&& (y4 === Std.( % ) !!false q4) ||| (q3 === Std.Nat.succ q5 &&& (y4 === Std.( % ) q6 q4) &&& elemo q4 q5)))
          )
      and evaloEvalo y6 y7 y8 = __evalo y6 y7 &&& _evalo y6 y8
      and _evaloEvalo y9 y10 y11 = __evalo y9 y10 &&& __evalo y9 y11
      and elemo y12 y13 =
        fresh (q1 q2 q3) (y13 === Std.Nat.zero &&& (y12 === Std.( % ) !!false q1) ||| (y13 === Std.Nat.succ q2 &&& (y12 === Std.( % ) q3 q1) &&& elemo q1 q2))
      and __evaloEvalo y14 y15 y16 = _evalo y14 y15 &&& __evalo y14 y16
      and ___evaloEvalo y17 y18 y19 = _evalo y17 y18 &&& _evalo y17 y19
      and _elemo y20 y21 =
        fresh (q1 q2 q3) (y21 === Std.Nat.zero &&& (y20 === Std.( % ) !!true q1) ||| (y21 === Std.Nat.succ q2 &&& (y20 === Std.( % ) q3 q1) &&& _elemo q1 q2))
      in
      _evalo x0 x1

    let _ =
      let x = Std.nat 0 in
      let y = Std.nat 1 in

      run_st "not x && x" @@
      run q (fun st ->
        topLevel st (conj (neg @@ var x) (var x))) ;

      run_st "not x && y" @@
      run q (fun st ->
        topLevel st (conj (neg @@ var x) (var y)));

      run_st "not x || x" @@
      run q (fun st ->
        topLevel st (disj (neg @@ var x) (var x)));


    (*
      (* won't terminate *)
      run_fm1 "\\forall x . not x && x" @@
      run qr (fun st r ->
        topLevel st (conj (neg @@ var r) (var r))) ;
    *)

      run_st2 "\\forall x y . not x && y" @@
      run qrs (fun st r t ->
        topLevel st (conj (neg @@ var r) (var t))) ;

      run_st1 "\\forall x . not x || x" @@
      run qr (fun st r ->
        topLevel st (disj (neg @@ var r) (var r))) ;

      run_formula 10000 "[x, y]" @@
      run qrs (fun r t fm ->
        topLevel (Std.(%<) r t) fm)

  end *)