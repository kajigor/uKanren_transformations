open GT
open OCanren
open OCanren.Stream

let (let*) = OCanren.Stream.bind
let return = OCanren.Stream.single
let mzero = OCanren.Stream.nil
let mplus = OCanren.Stream.mplus
let msum xs = List.fold_right mplus xs mzero
let guard p = if p then return () else mzero
let make_lazy f = OCanren.Stream.from_fun f
type term =
  | A
  | B
  | C
  | Cons of (term * term)
  | D
  | Nil

let rec revI x0  =
  msum
    [(let* x5 = return Nil in
    let* (x1, x2) = match x0 with
      | Cons (y1, y2) ->  return (y1, y2)
      | _ -> mzero in
    let* x4 = return (Cons (x1, x5)) in
    let* _ = _revII x2 x4  in return ())]
and _revII x0 x1  =
  msum
    [(let* x6 = return A in
    let* x8 = return B in
    let* x10 = return C in
    let* x12 = return D in
    let* x13 = return Nil in
    let* x11 = return (Cons (x12, x13)) in
    let* x9 = return (Cons (x10, x11)) in
    let* x7 = return (Cons (x8, x9)) in
    let* (x14, x15) = match x1 with
      | Cons (y14, y15) ->  return (y14, y15)
      | _ -> mzero in
    let* _ = guard (x14 == x6) in
    let* _ = guard (x15 == x7) in
    let* _ = guard (x0 == Nil) in return ());
    (let* (x2, x3) = match x0 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* x16 = return (Cons (x2, x1)) in
    let* _ = _revII x3 x16  in return ())]
and revO  gen__revOI_x2 gen_revO_x1 =
  msum
    [(let* x5 = return Nil in
    let* (x4, x1) = let* x1 = gen_revO_x1 in
    let* x4 = return (Cons (x1, x5)) in return (x4, x1) in
    let* x2 = _revOI x4 gen__revOI_x2 in
    let* x0 = return (Cons (x1, x2)) in return x0)]
and _revOI x1 gen__revOI_x2 =
  msum
    [(let* x6 = return A in
    let* x8 = return B in
    let* x10 = return C in
    let* x12 = return D in
    let* x13 = return Nil in
    let* x11 = return (Cons (x12, x13)) in
    let* x9 = return (Cons (x10, x11)) in
    let* x7 = return (Cons (x8, x9)) in
    let* x0 = return Nil in
    let* (x14, x15) = match x1 with
      | Cons (y14, y15) ->  return (y14, y15)
      | _ -> mzero in
    let* _ = guard (x14 == x6) in
    let* _ = guard (x15 == x7) in return x0);
    (let* (x16, x2) = let* x2 = gen__revOI_x2 in
    let* x16 = return (Cons (x2, x1)) in return (x16, x2) in
    let* x3 = _revOI x16 gen__revOI_x2 in
    let* x0 = return (Cons (x2, x3)) in return x0)]