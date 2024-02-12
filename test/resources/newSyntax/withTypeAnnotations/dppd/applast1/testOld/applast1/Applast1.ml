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

let rec applastoI x0  =
  msum
    [(let* (x1, x2) = match x0 with
      | Cons (y1, y2) ->  return (y1, y2)
      | _ -> mzero in
    let* _ = appendoLastoII x1 x2  in return ())]
and appendoLastoII x0 x1  =
  msum
    [(let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = appendoLastoII x2 x3  in return ())]
and applastoO  gen_appendoLastoOO_x0 =
  msum
    [(let* (x1, x2) = appendoLastoOO  gen_appendoLastoOO_x0 in
    let* x0 = return (Cons (x1, x2)) in return x0)]
and appendoLastoOO  gen_appendoLastoOO_x0 =
  msum
    [(let* (x2, x3) = appendoLastoOO  gen_appendoLastoOO_x0 in
    let* x1 = return (Cons (x2, x3)) in
    let* x0 = gen_appendoLastoOO_x0 in return (x0, x1))]