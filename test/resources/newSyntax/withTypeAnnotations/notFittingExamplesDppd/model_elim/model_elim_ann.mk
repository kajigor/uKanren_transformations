filter (dynamic)
 input_clause clause =
  ((fresh l in
    (clause == [Pos (App [] l l)])) |
   (fresh h, x, y, z in
    (clause == [Pos (App (h :: x) y (h :: z)), Neg (App x y z)])));

filter (static dynamic)
 neg from to =
  ((fresh f in
    ((from == Neg f & to == Pos f))) |
   (fresh f in
    ((from == Pos f & to == Neg f))));

filter (static static)
 member x lst =
  ((fresh t in (lst == (x :: t))) |
   (fresh h, t in ((lst == (h :: t) & Unfold member x t))));

filter (static static dynamic)
 contrapositive1 g ys xs =
  (ys == (g :: xs) |
  (fresh x, t1, t2 in
    ((ys == (x :: t1) & xs == (x :: t2) & Unfold contrapositive1 g t1 t2))));

filter (static)
 contrapositive x =
  ((fresh g, b in
    ((x == Impl g b & Unfold input_clause ((g :: b))))) |
   (fresh g, b, bs1, bs in
    ((x == Impl g (b :: bs1) & Unfold input_clause ((b :: bs)) & Unfold contrapositive1 g bs bs1))));

filter (static static)
 proveall lst a =
  (lst == [] |
  (fresh h, t in
    ((lst == (h :: t) & Memo prove h a & Unfold proveall t a))));

filter (static static)
 prove g a =
  (Unfold member g a |
  (fresh gn, b in
    ((Unfold neg g gn & Unfold contrapositive (Impl gn b) & Unfold proveall b ((gn :: a))))));

filter (static static)
 solve from goal =
  ((fresh g1, g2 in
    ((from == Pair g1 g2 & goal == [] & Unfold solve g1 [] & Unfold solve g2 []))) |
    Unfold prove from goal);

filter ()
 fail  = Memo fail [];

(fresh x, y, z in (Unfold solve (Neg (App x y z)) []))