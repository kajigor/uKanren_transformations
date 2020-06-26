open GT
open OCanren
open OCanren.Std
type ('a2, 'a1, 'a0) gldb =
  | Iconst_ of 'a1 
  | Bconst_ of 'a2 
  | Var_ of 'a1 
  | Plus_ of 'a0 * 'a0 
  | Mult_ of 'a0 * 'a0 
  | Equal_ of 'a0 * 'a0 
  | Less_ of 'a0 * 'a0 
  | If_ of 'a0 * 'a0 * 'a0 
  | Let_ of 'a0 * 'a0 
module For_gldb =
  (Fmap3)(struct
            let rec fmap fa2 fa1 fa0 =
              function
              | Iconst_ a1 -> Iconst_ (fa1 a1)
              | Bconst_ a2 -> Bconst_ (fa2 a2)
              | Var_ a1 -> Var_ (fa1 a1)
              | Plus_ (a0_0, a0_1) -> Plus_ ((fa0 a0_0), (fa0 a0_1))
              | Mult_ (a0_0, a0_1) -> Mult_ ((fa0 a0_0), (fa0 a0_1))
              | Equal_ (a0_0, a0_1) -> Equal_ ((fa0 a0_0), (fa0 a0_1))
              | Less_ (a0_0, a0_1) -> Less_ ((fa0 a0_0), (fa0 a0_1))
              | If_ (a0_0, a0_1, a0_2) -> If_ ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2))
              | Let_ (a0_0, a0_1) -> Let_ ((fa0 a0_0), (fa0 a0_1))
            type ('a2, 'a1, 'a0) t = ('a2, 'a1, 'a0) gldb
          end)
let rec iconst_ x__0 = inj (For_gldb.distrib (Iconst_ x__0))
and bconst_ x__0 = inj (For_gldb.distrib (Bconst_ x__0))
and var_ x__0 = inj (For_gldb.distrib (Var_ x__0))
and plus_ x__0 x__1 = inj (For_gldb.distrib (Plus_ (x__0, x__1)))
and mult_ x__0 x__1 = inj (For_gldb.distrib (Mult_ (x__0, x__1)))
and equal_ x__0 x__1 = inj (For_gldb.distrib (Equal_ (x__0, x__1)))
and less_ x__0 x__1 = inj (For_gldb.distrib (Less_ (x__0, x__1)))
and if_ x__0 x__1 x__2 = inj (For_gldb.distrib (If_ (x__0, x__1, x__2)))
and let_ x__0 x__1 = inj (For_gldb.distrib (Let_ (x__0, x__1)))
type type_ =
  | Int 
  | Bool 
let int () = !! Int
let bool () = !! Bool
let (>>=) e f q50 = ((e === (none ())) &&& (q50 === (none ()))) ||| (fresh (x) (e === (some x)) (f x q50))
let chainIfEqual e v f q47 = fresh (q48) ((==) (some v) e q48) (conde [(q48 === (!! true)) &&& (f v q47); (q48 === (!! false)) &&& (q47 === (none ()))])
let rec typecheck_ gamma term q0 =
  conde
    [fresh (q1) (term === (iconst_ q1)) (q0 === (some (int ())));
    fresh (q3) (term === (bconst_ q3)) (q0 === (some (bool ())));
    fresh (v) (term === (var_ v)) (List.nth_opt gamma v q0);
    fresh (x y) (term === (plus_ x y))
      ((@@) (fun q7 -> fun q6 -> fresh (q5) (typecheck_ gamma x q5) (chainIfEqual q5 (int ()) q7 q6))
         (fun k -> (fun q10 -> fun q9 -> fresh (q8) (typecheck_ gamma y q8) (chainIfEqual q8 (int ()) q10 q9)) @@ (fun l -> fun q11 -> q11 === (some (int ())))) q0);
    fresh (x y) (term === (mult_ x y))
      ((@@) (fun q14 -> fun q13 -> fresh (q12) (typecheck_ gamma x q12) (chainIfEqual q12 (int ()) q14 q13))
         (fun k -> (fun q17 -> fun q16 -> fresh (q15) (typecheck_ gamma y q15) (chainIfEqual q15 (int ()) q17 q16)) @@ (fun l -> fun q18 -> q18 === (some (int ())))) q0);
    fresh (x y q19) (term === (equal_ x y)) (typecheck_ gamma x q19)
      ((>>=) q19
         (fun x' ->
            fun q25 ->
              fresh (q20) (typecheck_ gamma y q20)
                ((>>=) q20
                   (fun y' -> fun q21 -> fresh (q22) ((==) x' y' q22) (conde [(q22 === (!! true)) &&& (q21 === (some (bool ()))); (q22 === (!! false)) &&& (q21 === (none ()))]))
                   q25))
         q0);
    fresh (x y) (term === (less_ x y))
      ((@@) (fun q29 -> fun q28 -> fresh (q27) (typecheck_ gamma x q27) (chainIfEqual q27 (int ()) q29 q28))
         (fun k -> (fun q32 -> fun q31 -> fresh (q30) (typecheck_ gamma y q30) (chainIfEqual q30 (int ()) q32 q31)) @@ (fun l -> fun q33 -> q33 === (some (bool ())))) q0);
    fresh (c t e) (term === (if_ c t e))
      ((@@) (fun q36 -> fun q35 -> fresh (q34) (typecheck_ gamma c q34) (chainIfEqual q34 (bool ()) q36 q35))
         (fun l ->
            fun q44 ->
              fresh (q37) (typecheck_ gamma t q37)
                ((>>=) q37
                   (fun t' ->
                      fun q43 ->
                        fresh (q38) (typecheck_ gamma e q38)
                          ((>>=) q38
                             (fun e' -> fun q39 -> fresh (q40) ((==) t' e' q40) (conde [(q40 === (!! true)) &&& (q39 === (some t')); (q40 === (!! false)) &&& (q39 === (none ()))]))
                             q43))
                   q44))
         q0);
    fresh (v b q45) (term === (let_ v b)) (typecheck_ gamma v q45) ((>>=) q45 (fun v' -> typecheck_ (v' % gamma) b) q0)]