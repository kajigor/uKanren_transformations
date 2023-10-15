filter ()
 fail  = Memo fail [];
filter (dynamic static dynamic)
 addo x y z = ((x == Zero & z == y) | (fresh x' in ((x == Succ x' & Memo addo x' (Succ y) z))));

(fresh x, y, z in (Unfold addo x (Succ (Succ Zero)) z))