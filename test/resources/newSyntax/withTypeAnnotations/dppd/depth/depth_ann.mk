filter (static dynamic)
 prog_clause exp body =
  ((fresh x, xs, y, z in
    ((exp == Member x xs &
      body == Append y (x :: z) xs))) |
  (fresh x in
    ((exp == Append [] x x &
      body == True))) |
  (fresh x, l1, l2, l3 in
    ((exp == Append (x :: l1) l2 (x :: l3) &
    body == Append l1 l2 l3))));

filter (static static dynamic)
 max a b mx =
  ((b == O & a == mx) |
  (a == O & b == mx) |
  (fresh a1, b1, mx1 in
    ((a == S a1 &
      b == S b1 &
      mx == S mx1 &
      Unfold max a1 b1 mx1))));

filter (static dynamic)
 depth exp d =
  ((exp == True & d == O) |
  (fresh l, r, d1, d2 in
    ((exp == Cons l r &
      Unfold depth l d1 &
      Unfold depth r d2 &
      Unfold max d1 d2 d))) |
  (fresh body, d1 in
    ((d == S d1 &
    Unfold prog_clause exp body &
    Memo depth body d1))));

filter ()
 fail  = Memo fail [];

(fresh exp, x, d in (
  Unfold depth (Member O [S O, S (S O), S (S O), S (S (S O)), S O, O, S (S O), O, S O, S (S O), O, O]) d))
