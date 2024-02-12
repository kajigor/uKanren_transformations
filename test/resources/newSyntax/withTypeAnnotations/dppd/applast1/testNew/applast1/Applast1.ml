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
  | Cons of (term * term)
  | Nil
  | O
  | S of term

let rec applastoI x0  =
  msum
    [(let* x6 = return O in
    let* x5 = return (S x6) in
    let* x8 = return Nil in
    let* (x7, x1) = match x0 with
      | Cons (y7, y1) ->  return (y7, y1)
      | _ -> mzero in
    let* _ = guard (x7 == x5) in
    let* _ = _appendoII x1 x8  in return ());
    (let* (x2, x1) = match x0 with
      | Cons (y2, y1) ->  return (y2, y1)
      | _ -> mzero in
    let* _ = appendoLastoI x1  in return ())]
and _appendoII x0 x1  =
  msum
    [(let* x10 = return O in
    let* x11 = return Nil in
    let* (x12, x13) = match x1 with
      | Cons (y12, y13) ->  return (y12, y13)
      | _ -> mzero in
    let* _ = guard (x12 == x10) in
    let* _ = guard (x13 == x11) in
    let* _ = guard (x0 == Nil) in return ());
    (let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* (x14, x4) = match x0 with
      | Cons (y14, y4) ->  return (y14, y4)
      | _ -> mzero in
    let* _ = guard (x14 == x2) in
    let* _ = _appendoII x4 x3  in return ())]
and appendoLastoI x0  =
  msum
    [(let* (x1, x2) = match x0 with
      | Cons (y1, y2) ->  return (y1, y2)
      | _ -> mzero in
    let* x3 = _appendoIO x2  in
    let* x9 = return (Cons (x1, x3)) in
    let* _ = lastoI x9  in return ())]
and _appendoIO x0  =
  msum
    [(let* x10 = return O in
    let* x11 = return Nil in
    let* _ = guard (x0 == Nil) in
    let* x12 = return x10 in
    let* x13 = return x11 in
    let* x1 = return (Cons (x12, x13)) in return x1);
    (let* (x14, x4) = match x0 with
      | Cons (y14, y4) ->  return (y14, y4)
      | _ -> mzero in
    let* x2 = return x14 in
    let* x3 = _appendoIO x4  in
    let* x1 = return (Cons (x2, x3)) in return x1)]
and applastoO  gen_applastoO_x2 gen_lastoO_x1 =
  msum
    [(let* x6 = return O in
    let* x5 = return (S x6) in
    let* x8 = return Nil in
    let* x7 = return x5 in
    let* x1 = _appendoOI x8  in
    let* x0 = return (Cons (x7, x1)) in return x0);
    (let* x1 = appendoLastoO  gen_lastoO_x1 in
    let* (x0, x2) = let* x2 = gen_applastoO_x2 in
    let* x0 = return (Cons (x2, x1)) in return (x0, x2) in return x0)]
and _appendoOI x1  =
  msum
    [(let* x10 = return O in
    let* x11 = return Nil in
    let* x0 = return Nil in
    let* (x12, x13) = match x1 with
      | Cons (y12, y13) ->  return (y12, y13)
      | _ -> mzero in
    let* _ = guard (x12 == x10) in
    let* _ = guard (x13 == x11) in return x0);
    (let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* x14 = return x2 in
    let* x4 = _appendoOI x3  in
    let* x0 = return (Cons (x14, x4)) in return x0)]
and appendoLastoO  gen_lastoO_x1 =
  msum
    [(let* x9 = lastoO  gen_lastoO_x1 in
    let* (x1, x3) = match x9 with
      | Cons (y1, y3) ->  return (y1, y3)
      | _ -> mzero in
    let* x2 = _appendoOI x3  in
    let* x0 = return (Cons (x1, x2)) in return x0)]
and lastoI x0  =
  msum
    [(let* x16 = return O in
    let* x15 = return (S x16) in
    let* x17 = return Nil in
    let* (x18, x19) = match x0 with
      | Cons (y18, y19) ->  return (y18, y19)
      | _ -> mzero in
    let* _ = guard (x18 == x15) in
    let* _ = guard (x19 == x17) in return ());
    (let* (x1, x2) = match x0 with
      | Cons (y1, y2) ->  return (y1, y2)
      | _ -> mzero in
    let* _ = lastoI x2  in return ())]
and lastoO  gen_lastoO_x1 =
  msum
    [(let* x16 = return O in
    let* x15 = return (S x16) in
    let* x17 = return Nil in
    let* x18 = return x15 in
    let* x19 = return x17 in
    let* x0 = return (Cons (x18, x19)) in return x0);
    (let* x2 = lastoO  gen_lastoO_x1 in
    let* (x0, x1) = let* x1 = gen_lastoO_x1 in
    let* x0 = return (Cons (x1, x2)) in return (x0, x1) in return x0)]