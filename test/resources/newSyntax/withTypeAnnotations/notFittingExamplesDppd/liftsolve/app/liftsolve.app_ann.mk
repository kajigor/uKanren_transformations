filter (static dynamic static dynamic)
 l_mkng lst1 lst2 inSub outSub =
  ((lst1 == [] & lst2 == [] & inSub == outSub) |
   (fresh h, t, ih, it, intSub in
    ((lst1 == (h :: t) & Unfold mkng h ih inSub intSub & Memo l_mkng t it intSub outSub & lst2 == (ih :: it)))));

filter (static dynamic static dynamic)
 mkng term1 term2 inSub outSub =
  ((fresh n in
    ((term1 == Var n & inSub == [] & outSub == [Sub n term2]))) |
   (fresh n, t in
    ((term1 == Var n & inSub == (Sub n term2 :: t) & outSub == (Sub n term2 :: t)))) |
   (fresh n, m, t, t1, y in
    ((term1 == Var n & inSub == (Sub m y :: t) & outSub == (Sub m y :: t1) & Unfold neq n m & Unfold mkng term1 term2 t t1))) |
   (fresh f, args, iArgs in
    ((term1 == Term f args & term2 == Term f iArgs & Unfold l_mkng args iArgs inSub outSub))));

filter (static static)
 neq x y =
  ((fresh t in
    ((x == Zero & y == Succ t))) |
   (fresh t in
    ((x == Succ t & y == Zero))) |
   (fresh t1, t2 in
    ((x == Succ t1 & y == Succ t2 & Unfold neq t1 t2))));

filter (static dynamic)
 make_non_ground g ng =
  (fresh sub in
    (Unfold mkng g ng [] sub));

filter (dynamic static)
 non_ground_member ngx ngl =
  (fresh grh, grt in
    ((ngl == (grh :: grt) & (Unfold non_ground_member ngx grt | Unfold make_non_ground grh ngx))));

filter (static static)
 solve rules res =
  (res == [] |
  (fresh ngh, ngt, clause, ngbody, clause in
    ((res == (ngh :: ngt) & Unfold solve rules ngt & Unfold non_ground_member (Term clause (ngh :: ngbody)) rules & Memo solve rules ngbody))));

filter ()
 fail  = Memo fail [];

(fresh clause, app, null, l, cons, h, x, y, z in
  Unfold solve [Term clause [Term app [Term null Nil, Var l, Var l]],
           Term clause [Term app [Term cons [Var h, Var x], Var y, Term cons [Var h, Var z]], Term app [Var x, Var y, Var z]]] [Term app [x, y, z]])