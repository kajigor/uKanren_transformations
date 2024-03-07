match pat t =
  match1 pat t pat t;

neq a b =
  (
    fresh t in
    (
      a == Succ t &
      b == Zero
    )
  ) |
  (
    fresh t in
    (
      b == Succ t &
      a == Zero
    )
  ) |
  (
    fresh t, t1 in
    (
      a == Succ t &
      b == Succ t1 &
      neq t t1
    )
  );

match1 patl tl pat t =
  patl == Nil |
  (
    fresh a, b, ps, ts in
    (
      patl == (a :: ps) &
      tl == (b :: ts) &
      (
        a == b &
        match1 ps ts pat t |
        (
          fresh x, t1 in
            (t == (x :: t1) &
            neq a b &
            match1 pat t1 pat t1)
        )
      )
    )
  );

? match x ([S (S O), S O, S O, O, O, S (S O), O, S O])