filter (dynamic dynamic static)
multiply x y z = 
  ((y == Zero & z == Zero) | 
  (fresh t, z1 in 
    ((y == Succ t & 
      Unfold add x z1 z & 
      Memo multiply x t z1))));

filter (dynamic dynamic static)
 add x y z = ((x == Zero & y == z) | (fresh t1, t2 in ((x == Succ t1 & z == Succ t2 & Unfold add t1 y t2))));

filter ()
 fail  = Memo fail [];

(fresh x, y in (Unfold multiply x y (Succ (Succ (Succ (Succ Zero))))))