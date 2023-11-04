filter (static static dynamic)
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

filter (static dynamic)
claus exp body =
  (
    fresh x, t in
      exp == Member x (x :: t) &
      body == Nil
  ) |
  (
    fresh x, t, y in
      exp == Member x (y :: t) &
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
    fresh h, x, y, z in
      exp == App (h :: x) y (h :: z) &
      body == [App x y z]
  ) |
  (
    fresh x, t in
      exp == Delete x (x :: t) t &
      body == Nil
  ) |
  (
    fresh x, y, t, d in
      exp == Delete x (y :: t) (y :: d) &
      body == [Delete x t d]
  ) |
  (
    fresh a, l1, l2, res, d1 in
      exp == Test a l1 l2 res &
      body == [InBoth a l1 l2, Delete a l1 d1, App d1 l2 res]
  );

filter (static dynamic)
solve lst res =
  solve1 lst O res;

? solve ([Test O ([S (S O), S O, O, O, S (S O), S O]) ([O, S O, S (S O), O]) y]) x