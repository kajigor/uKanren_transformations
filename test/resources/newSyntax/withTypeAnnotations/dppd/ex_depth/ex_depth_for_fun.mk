solve1 lst depth res =
  lst == Nil &
  depth == res |
  (
    fresh head, tail, body, intDepth in
      lst == (head :: tail) &
      claus head body &
      solve1 body (S depth) intDepth &
      solve1 tail intDepth res
  );

claus exp body =
  (
    fresh x, t, tx in
      exp == Member x tx &
      tx == (x :: t) &
      body == Nil
  ) |
  (
    fresh x, t, y, ty in
      exp == Member x ty &
      ty == (y :: t) &
      body == [Member x t]
  ) |
  (
    fresh x, l1, l2 in
      exp == InBoth x l1 l2 &
      body == [Member x l1, Member x l2]
  ) |
  (
    fresh l, l1 in
      l1 == Nil &
      exp == App l1 l l &
      body == Nil
  ) |
  (
    fresh h, x, y, z, hx, hz in
      exp == App hx y hz &
      hx == (h :: x) &
      hz == (h :: z) &
      body == [App x y z]
  ) |
  (
    fresh x, t, xt in
      exp == Delete x xt t &
      xt == (x :: t) &
      body == Nil
  ) |
  (
    fresh x, y, t, d, yt, yd in
      exp == Delete x yt yd &
      yt == (y :: t) &
      yd == (y :: d) &
      body == [Delete x t d]
  ) |
  (
    fresh a, l1, l2, res, d1 in
      exp == Test a l1 l2 res &
      body == [InBoth a l1 l2, Delete a l1 d1, App d1 l2 res]
  );

solve lst res =
  solve1 lst O res;

help x y =
	solve ([Test O ([S (S O), S O, O, O, S (S O), S O]) ([O, S O, S (S O), O]) y]) x;

? help x y