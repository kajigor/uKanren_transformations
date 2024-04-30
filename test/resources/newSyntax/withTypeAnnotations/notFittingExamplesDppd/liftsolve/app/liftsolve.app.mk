filter (static static)
solve rules res =
  res == Nil |
  (
    fresh ngh, ngt, clause, ngbody, clause in
      res == (ngh :: ngt) &
      solve rules ngt &
      non_ground_member (Term clause (ngh :: ngbody)) rules &
      solve rules ngbody
  );

filter (dynamic static)
non_ground_member ngx ngl =
  (
    fresh grh, grt in
      ngl == (grh :: grt) &
      (
        non_ground_member ngx grt |
        make_non_ground grh ngx
      )
  );

filter (static dynamic)
make_non_ground g ng =
  (
    fresh sub in
      mkng g ng Nil sub
  );


filter (static static)
neq x y =
  (
    fresh t in
      x == Zero &
      y == Succ t
  ) |
  (
    fresh t in
      x == Succ t &
      y == Zero
  ) |
  (
    fresh t1, t2 in
      x == Succ t1 &
      y == Succ t2 &
      neq t1 t2
  );


filter (static dynamic static dynamic)
mkng term1 term2 inSub outSub =
  (
    fresh n in
      term1 == Var n &
      inSub == Nil &
      outSub == [Sub n term2]
  ) |
  (
    fresh n, t in
      term1 == Var n &
      inSub == (Sub n term2 :: t) &
      outSub == (Sub n term2 :: t)
  ) |
  (
    fresh n, m, t, t1, y in
      term1 == Var n &
      inSub == (Sub m y :: t) &
      outSub == (Sub m y :: t1) &
      neq n m &
      mkng term1 term2 t t1
  ) |
  (
    fresh f, args, iArgs in
      term1 == Term f args &
      term2 == Term f iArgs &
      l_mkng args iArgs inSub outSub
  );

filter (static dynamic static dynamic)
l_mkng lst1 lst2 inSub outSub =
  lst1 == Nil &
  lst2 == Nil &
  inSub == outSub |
  (
    fresh h, t, ih, it, intSub in
      lst1 == (h :: t) &
      mkng h ih inSub intSub &
      l_mkng t it intSub outSub &
      lst2 == (ih :: it)
  );

? solve [Term Clause [Term App [Term Null Nil, Var O, Var O]],
         Term Clause [Term App [Term Cons [Var O, Var (S O)], Var (S (S O)), Term Cons [Var O, Var (S (S (S O)))]], Term App [Var (S O), Var (S (S O)), Var (S (S (S O)))]]] [Term App [x, y, z]]