open GT
open OCanren
open OCanren.Std
open Helper 

let type_eq x y q86 =
  fresh (q87) (q87 === (pair x y))
    (conde
       [(q87 === (pair (int ()) (int ()))) &&& (q86 === (!! true));
       (q87 === (pair (bool ()) (bool ()))) &&& (q86 === (!! true));
       (q87 === (pair (int ()) (bool ()))) &&& (q86 === (!! false));
       (q87 === (pair (bool ()) (int ()))) &&& (q86 === (!! false))])
let rec nth_opt xs n q82 =
  ((xs === (nil ())) &&& (q82 === (none ()))) ||| (fresh (h t) (xs === (h % t)) (((n === (o ())) &&& (q82 === (some h))) ||| (fresh (x) (n === (s x)) (nth_opt t x q82))))
let rec typecheck_ gamma term q0 =
  conde
    [fresh (q1) (term === (iconst_ q1)) (q0 === (some (int ())));
    fresh (q3) (term === (bconst_ q3)) (q0 === (some (bool ())));
    fresh (v) (term === (var_ v)) (nth_opt gamma v q0);
    fresh (x y q6) (term === (plus_ x y)) (typecheck_ gamma x q6)
      (((q6 === (none ())) &&& (q0 === (none ()))) |||
         (fresh (x' q9) (q6 === (some x')) (typecheck_ gamma y q9)
            (((q9 === (none ())) &&& (q0 === (none ()))) |||
               (fresh (y' q12 q15 q16) (q9 === (some y')) (type_eq x' (int ()) q15) (
                  type_eq y' (int ()) q16) (conde [(q15 === (!! false)) &&& (q12 === (!! false)); (q15 === (!! true)) &&& (q12 === q16)])
                  (conde [(q12 === (!! true)) &&& (q0 === (some (int ()))); (q12 === (!! false)) &&& (q0 === (none ()))])))));
    fresh (x y q22) (term === (mult_ x y)) (typecheck_ gamma x q22)
      (((q22 === (none ())) &&& (q0 === (none ()))) |||
         (fresh (x' q25) (q22 === (some x')) (typecheck_ gamma y q25)
            (((q25 === (none ())) &&& (q0 === (none ()))) |||
               (fresh (y' q28 q31 q32) (q25 === (some y')) (type_eq x' (int ()) q31) (
                  type_eq y' (int ()) q32) (conde [(q31 === (!! false)) &&& (q28 === (!! false)); (q31 === (!! true)) &&& (q28 === q32)])
                  (conde [(q28 === (!! true)) &&& (q0 === (some (int ()))); (q28 === (!! false)) &&& (q0 === (none ()))])))));
    fresh (x y q38) (term === (equal_ x y)) (typecheck_ gamma x q38)
      (((q38 === (none ())) &&& (q0 === (none ()))) |||
         (fresh (x' q41) (q38 === (some x')) (typecheck_ gamma y q41)
            (((q41 === (none ())) &&& (q0 === (none ()))) |||
               (fresh (y' q44) (q41 === (some y')) (type_eq x' y' q44) (conde [(q44 === (!! true)) &&& (q0 === (some (bool ()))); (q44 === (!! false)) &&& (q0 === (none ()))])))));
    fresh (x y q48) (term === (less_ x y)) (typecheck_ gamma x q48)
      (((q48 === (none ())) &&& (q0 === (none ()))) |||
         (fresh (x' q51) (q48 === (some x')) (typecheck_ gamma y q51)
            (((q51 === (none ())) &&& (q0 === (none ()))) |||
               (fresh (y' q54 q57 q58) (q51 === (some y')) (type_eq x' (int ()) q57) (
                  type_eq y' (int ()) q58) (conde [(q57 === (!! false)) &&& (q54 === (!! false)); (q57 === (!! true)) &&& (q54 === q58)])
                  (conde [(q54 === (!! true)) &&& (q0 === (some (bool ()))); (q54 === (!! false)) &&& (q0 === (none ()))])))));
    fresh (c t e q64) (term === (if_ c t e)) (typecheck_ gamma c q64)
      (((q64 === (none ())) &&& (q0 === (none ()))) |||
         (fresh (c' q67) (q64 === (some c')) (type_eq c' (bool ()) q67)
            (conde
               [fresh (q69) (q67 === (!! true)) (typecheck_ gamma t q69)
                  (((q69 === (none ())) &&& (q0 === (none ()))) |||
                     (fresh (t' q72) (q69 === (some t')) (typecheck_ gamma e q72)
                        (((q72 === (none ())) &&& (q0 === (none ()))) |||
                           (fresh (e' q75) (q72 === (some e')) (type_eq t' e' q75)
                              (conde [(q75 === (!! true)) &&& (q0 === (some t')); (q75 === (!! false)) &&& (q0 === (none ()))])))));
               (q67 === (!! false)) &&& (q0 === (none ()))])));
    fresh (v b q80) (term === (let_ v b)) (typecheck_ gamma v q80)
      (((q80 === (none ())) &&& (q0 === (none ()))) ||| (fresh (v') (q80 === (some v')) (typecheck_ (v' % gamma) b q0)))]
