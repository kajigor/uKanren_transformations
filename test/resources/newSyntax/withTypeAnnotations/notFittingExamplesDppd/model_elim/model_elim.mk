filter (static static)
solve from goal =
  (
    fresh g1, g2 in
      from == Pair g1 g2 &
      goal == Nil &
      solve g1 Nil &
      solve g2 Nil
  ) |
  prove from goal;

filter (static static)
prove g a =
  member g a |
  (
    fresh gn, b in
      neg g gn &
      contrapositive (Impl gn b) &
      proveall b (gn :: a)
  );

filter (static static)
proveall lst a =
  lst == Nil |
  (
    fresh h, t in
      lst == (h :: t) &
      prove h a &
      proveall t a
  );

filter (static)
contrapositive x =
  (
    fresh g, b in
      x == Impl g b &
      input_clause (g :: b)
  ) |
  (
    fresh g, b, bs1, bs in
      x == Impl g (b :: bs1) &
      input_clause (b :: bs) &
      contrapositive1 g bs bs1
  );

filter (static static dynamic)
contrapositive1 g ys xs =
    ys == (g :: xs) |
    (
      fresh x, t1, t2 in
        ys == (x :: t1) &
        xs == (x :: t2) &
        contrapositive1 g t1 t2
    );

filter (static static)
member x lst =
  (
    fresh t in
      lst == (x :: t)
  ) |
  (
    fresh h, t in
      lst == (h :: t) &
      member x t
  );

filter (static dynamic)
neg from to =
  (
    fresh f in
      from == Neg f &
      to == Pos f
  ) |
  (
    fresh f in
      from == Pos f &
      to == Neg f
  );

filter (dynamic)
input_clause clause =
  (
    fresh l in
      clause == [Pos (App Nil l l )]
  ) |
  (
    fresh h, x, y, z in
      clause == [Pos (App (h :: x) y (h :: z)), Neg  (App x y z)]
  );

? solve (Neg (App x y z)) Nil
