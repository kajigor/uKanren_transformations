open GT
open MiniKanren
open Std

let appendo2 x0 x1 x2 x3 x4 =
  let rec f1 x0 x1 x2 x3 x4 =
    fresh (x10 x5 x6 x7) (
        ((x0 === nil ()) &&& (x2 === x1)) &&&
           (((x1 === nil ()) &&& (x4 === x3)) |||
           ((x1 === x5 % x7) &&& (x4 === x5 % x10) &&&
            let rec f2 x9 x3 x10 =
              fresh (x11 x12 x13) (
                  ((x9 === nil ()) &&& (x10 === x3)) |||
                  ((x9 === x11 % x12) &&& (x10 === x11 % x13) &&& f2 x12 x3 x13)
                )
            in
            f2 x7 x3 x10)) |||
            ((x0 === x5 % x6) &&& (x2 === x5 % x7) &&& (x4 === x5 % x10) &&& f1 x6 x1 x7 x3 x10)
      )
  in
  f1 x0 x1 x2 x3 x4

let (!) x = inj @@ lift x
                        
let _ = run qr (fun q r -> appendo2 (!< !1) (!< !2 ) r (!< !3) q)
               (fun q r -> Stream.iter (fun l -> Printf.printf "%s\n" @@ (show(List.ground) (show(int))) @@ l#prj) q)
